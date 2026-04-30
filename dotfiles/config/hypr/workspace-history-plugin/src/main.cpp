#define WLR_USE_UNSTABLE

#include <hyprland/src/Compositor.hpp>
#include <hyprland/src/debug/log/Logger.hpp>
#include <hyprland/src/desktop/Workspace.hpp>
#include <hyprland/src/desktop/state/FocusState.hpp>
#include <hyprland/src/devices/IKeyboard.hpp>
#include <hyprland/src/event/EventBus.hpp>
#include <hyprland/src/helpers/Monitor.hpp>
#include <hyprland/src/managers/KeybindManager.hpp>
#include <hyprland/src/plugins/PluginAPI.hpp>

#include <lua.hpp>

#include <algorithm>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

inline HANDLE PHANDLE = nullptr;

namespace {

constexpr int MAX_WORKSPACE = 9;

struct SCycleState {
    std::string      monitorKey;
    int              originalWorkspace = 0;
    int              previewWorkspace  = 0;
    std::vector<int> history;
    size_t           nextIndex = 0;
};

class CWorkspaceHistory {
  public:
    void seedActiveWorkspaces() {
        if (!g_pCompositor)
            return;

        for (const auto& monitor : g_pCompositor->m_monitors) {
            if (monitor && monitor->m_activeWorkspace)
                remember(monitor->m_activeWorkspace);
        }

        writeDebug("seed");
    }

    void observe(PHLWORKSPACE workspace) {
        const auto workspaceId = workspaceID(workspace);
        if (!workspaceId) {
            writeDebug("observe-skipped-non-normal-workspace");
            return;
        }

        const auto key = monitorKey(workspace);
        if (!m_cycle || m_cycle->monitorKey != key) {
            remember(workspace);
            return;
        }

        if (contains(m_cycle->history, *workspaceId)) {
            m_cycle->previewWorkspace = *workspaceId;
            writeDebug("cycle-observe-preview");
            return;
        }

        m_cycle.reset();
        remember(workspace);
        writeDebug("remember-after-cycle-abandoned");
    }

    SDispatchResult cycle(int direction) {
        auto* cycle = startCycle();
        if (!cycle)
            return {};

        if (cycle->history.size() < 2) {
            writeDebug("cycle-skipped-short-history");
            return {};
        }

        const auto target = cycle->history[cycle->nextIndex];
        cycle->previewWorkspace = target;
        const auto result = focusWorkspace(target);
        if (!result.success) {
            writeDebug("cycle-focus-failed");
            return result;
        }

        cycle->nextIndex = wrappedIndex(cycle->nextIndex, direction, cycle->history.size());
        writeDebug(std::string("cycle-preview-") + std::to_string(target));
        return {};
    }

    SDispatchResult commit() {
        if (!m_cycle) {
            writeDebug("commit-skipped-no-cycle");
            return {};
        }

        auto cycle = *m_cycle;
        m_cycle.reset();
        m_histories[cycle.monitorKey] = promote(cycle.history, cycle.previewWorkspace);
        writeDebug("commit");
        return {};
    }

    SDispatchResult cancel() {
        if (!m_cycle) {
            writeDebug("cancel-skipped-no-cycle");
            return {};
        }

        const auto original = m_cycle->originalWorkspace;
        m_cycle.reset();
        focusWorkspace(original);
        writeDebug("cancel");
        return {};
    }

    void onKey(IKeyboard::SKeyEvent event) {
        if (!m_cycle || event.state != WL_KEYBOARD_KEY_STATE_RELEASED || !g_pKeybindManager)
            return;

        if (g_pKeybindManager->keycodeToModifier(event.keycode + 8) == HL_MODIFIER_META)
            commit();
    }

    std::string snapshot(const std::string& reason) const {
        std::stringstream out;
        out << "reason=" << reason << "\n";

        const auto monitor = Desktop::focusState() ? Desktop::focusState()->monitor() : nullptr;
        const auto workspace = monitor ? monitor->m_activeWorkspace : nullptr;
        out << "active_monitor=" << monitorKey(monitor) << "\n";
        out << "active_workspace=" << (workspace ? std::to_string(workspace->m_id) : "?") << "\n";

        for (const auto& [key, history] : m_histories) {
            out << "history." << key << "=";
            for (size_t i = 0; i < history.size(); ++i) {
                if (i > 0)
                    out << ",";
                out << history[i];
            }
            out << "\n";
        }

        if (m_cycle) {
            out << "cycle.monitor=" << m_cycle->monitorKey << "\n";
            out << "cycle.original=" << m_cycle->originalWorkspace << "\n";
            out << "cycle.preview=" << m_cycle->previewWorkspace << "\n";
            out << "cycle.next_index=" << (m_cycle->nextIndex + 1) << "\n";
            out << "cycle.history=";
            for (size_t i = 0; i < m_cycle->history.size(); ++i) {
                if (i > 0)
                    out << ",";
                out << m_cycle->history[i];
            }
            out << "\n";
        } else {
            out << "cycle=none\n";
        }

        return out.str();
    }

    void showDebug() const {
        HyprlandAPI::addNotification(PHANDLE, snapshot("notification"), CHyprColor{0.4, 0.8, 1.0, 1.0}, 6000);
    }

  private:
    std::map<std::string, std::vector<int>> m_histories;
    std::optional<SCycleState>              m_cycle;

    static std::optional<int> workspaceID(PHLWORKSPACE workspace) {
        if (!workspace || workspace->m_id < 1 || workspace->m_id > MAX_WORKSPACE)
            return std::nullopt;

        return workspace->m_id;
    }

    static std::string monitorKey(PHLMONITOR monitor) {
        if (!monitor)
            return "unknown";

        if (!monitor->m_name.empty())
            return monitor->m_name;

        return std::to_string(monitor->m_id);
    }

    static std::string monitorKey(PHLWORKSPACE workspace) {
        if (!workspace || !workspace->m_monitor)
            return monitorKey(Desktop::focusState() ? Desktop::focusState()->monitor() : nullptr);

        return monitorKey(workspace->m_monitor.lock());
    }

    static bool contains(const std::vector<int>& history, int workspace) {
        return std::ranges::contains(history, workspace);
    }

    static std::vector<int> promote(std::vector<int> history, int workspace) {
        std::erase(history, workspace);
        history.insert(history.begin(), workspace);
        return history;
    }

    static size_t wrappedIndex(size_t current, int direction, size_t size) {
        const auto signedSize = static_cast<int>(size);
        auto       next       = static_cast<int>(current) + direction;
        next                  = ((next % signedSize) + signedSize) % signedSize;
        return static_cast<size_t>(next);
    }

    PHLWORKSPACE activeWorkspace() const {
        const auto monitor = Desktop::focusState() ? Desktop::focusState()->monitor() : nullptr;
        return monitor ? monitor->m_activeWorkspace : nullptr;
    }

    void remember(PHLWORKSPACE workspace) {
        const auto workspaceId = workspaceID(workspace);
        if (!workspaceId) {
            writeDebug("remember-skipped-non-normal-workspace");
            return;
        }

        const auto key = monitorKey(workspace);
        auto&      history = m_histories[key];
        const bool changed = history.empty() || history.front() != *workspaceId;
        history            = promote(history, *workspaceId);

        if (changed)
            writeDebug("remember");
    }

    SCycleState* startCycle() {
        if (m_cycle)
            return &*m_cycle;

        const auto workspace = activeWorkspace();
        remember(workspace);

        const auto workspaceId = workspaceID(workspace);
        if (!workspaceId) {
            writeDebug("cycle-start-skipped-non-normal-workspace");
            return nullptr;
        }

        const auto key = monitorKey(workspace);
        auto       history = promote(m_histories[key], *workspaceId);

        if (history.size() < 2) {
            writeDebug("cycle-start-skipped-short-history");
            return nullptr;
        }

        m_cycle = SCycleState{
            .monitorKey         = key,
            .originalWorkspace  = *workspaceId,
            .previewWorkspace   = *workspaceId,
            .history            = history,
            .nextIndex          = 1,
        };
        writeDebug("cycle-start");
        return &*m_cycle;
    }

    static SDispatchResult focusWorkspace(int workspace) {
        if (!g_pKeybindManager)
            return {.success = false, .error = "keybind manager is unavailable"};

        const auto dispatcher = g_pKeybindManager->m_dispatchers.find("focusworkspaceoncurrentmonitor");
        if (dispatcher == g_pKeybindManager->m_dispatchers.end())
            return {.success = false, .error = "focusworkspaceoncurrentmonitor dispatcher is unavailable"};

        return dispatcher->second(std::to_string(workspace));
    }

    static std::optional<std::string> runtimePath(const std::string& name) {
        const auto runtimeDir = std::getenv("XDG_RUNTIME_DIR");
        if (!runtimeDir)
            return std::nullopt;

        return std::string(runtimeDir) + "/" + name;
    }

    void writeDebug(const std::string& reason) const {
        const auto statePath = runtimePath("hyprland-workspace-history-state");
        const auto logPath   = runtimePath("hyprland-workspace-history.log");
        if (!statePath || !logPath)
            return;

        const auto body = snapshot(reason);
        std::ofstream state(*statePath, std::ios::trunc);
        if (state)
            state << body;

        std::ofstream log(*logPath, std::ios::app);
        if (log) {
            const auto now = std::time(nullptr);
            log << "--- " << std::put_time(std::localtime(&now), "%Y-%m-%d %H:%M:%S") << " ---\n";
            log << body;
        }
    }
};

CWorkspaceHistory g_workspaceHistory;

SDispatchResult dispatchCycle(std::string arg) {
    int direction = 1;
    if (arg == "-1" || arg == "previous" || arg == "prev" || arg == "reverse")
        direction = -1;
    return g_workspaceHistory.cycle(direction);
}

SDispatchResult dispatchCommit(std::string) {
    return g_workspaceHistory.commit();
}

SDispatchResult dispatchCancel(std::string) {
    return g_workspaceHistory.cancel();
}

SDispatchResult dispatchDebug(std::string) {
    g_workspaceHistory.showDebug();
    return {};
}

int luaCycle(lua_State* L) {
    const auto result = g_workspaceHistory.cycle(static_cast<int>(luaL_optinteger(L, 1, 1)));
    if (!result.success)
        return luaL_error(L, "%s", result.error.c_str());
    return 0;
}

int luaCommit(lua_State* L) {
    const auto result = g_workspaceHistory.commit();
    if (!result.success)
        return luaL_error(L, "%s", result.error.c_str());
    return 0;
}

int luaCancel(lua_State* L) {
    const auto result = g_workspaceHistory.cancel();
    if (!result.success)
        return luaL_error(L, "%s", result.error.c_str());
    return 0;
}

int luaDebug(lua_State*) {
    g_workspaceHistory.showDebug();
    return 0;
}

void failNotification(const std::string& reason) {
    HyprlandAPI::addNotification(PHANDLE, "[workspace-history] " + reason, CHyprColor{1.0, 0.2, 0.2, 1.0}, 5000);
}

}

APICALL EXPORT std::string PLUGIN_API_VERSION() {
    return HYPRLAND_API_VERSION;
}

APICALL EXPORT PLUGIN_DESCRIPTION_INFO PLUGIN_INIT(HANDLE handle) {
    PHANDLE = handle;

    const std::string hash       = __hyprland_api_get_hash();
    const std::string clientHash = __hyprland_api_get_client_hash();
    if (hash != clientHash) {
        failNotification("version mismatch between Hyprland headers and running Hyprland");
        throw std::runtime_error("[workspace-history] version mismatch");
    }

    static auto workspaceHook = Event::bus()->m_events.workspace.active.listen([](PHLWORKSPACE workspace) { g_workspaceHistory.observe(workspace); });
    static auto keyboardHook = Event::bus()->m_events.input.keyboard.key.listen([](IKeyboard::SKeyEvent event, Event::SCallbackInfo&) { g_workspaceHistory.onKey(event); });
    static auto startHook = Event::bus()->m_events.start.listen([] { g_workspaceHistory.seedActiveWorkspaces(); });
    static auto reloadHook = Event::bus()->m_events.config.reloaded.listen([] { g_workspaceHistory.seedActiveWorkspaces(); });
    static auto monitorHook = Event::bus()->m_events.monitor.focused.listen([](PHLMONITOR monitor) {
        if (monitor && monitor->m_activeWorkspace)
            g_workspaceHistory.observe(monitor->m_activeWorkspace);
    });

    HyprlandAPI::addDispatcherV2(PHANDLE, "workspacehistory:cycle", ::dispatchCycle);
    HyprlandAPI::addDispatcherV2(PHANDLE, "workspacehistory:commit", ::dispatchCommit);
    HyprlandAPI::addDispatcherV2(PHANDLE, "workspacehistory:cancel", ::dispatchCancel);
    HyprlandAPI::addDispatcherV2(PHANDLE, "workspacehistory:debug", ::dispatchDebug);
    HyprlandAPI::addLuaFunction(PHANDLE, "workspacehistory", "cycle", ::luaCycle);
    HyprlandAPI::addLuaFunction(PHANDLE, "workspacehistory", "commit", ::luaCommit);
    HyprlandAPI::addLuaFunction(PHANDLE, "workspacehistory", "cancel", ::luaCancel);
    HyprlandAPI::addLuaFunction(PHANDLE, "workspacehistory", "debug", ::luaDebug);

    g_workspaceHistory.seedActiveWorkspaces();
    HyprlandAPI::addNotification(PHANDLE, "[workspace-history] Initialized", CHyprColor{0.2, 1.0, 0.2, 1.0}, 3000);
    return {"hypr-workspace-history", "Workspace history cycling with modifier-release commits", "Ivan Malison", "0.1.0"};
}

APICALL EXPORT void PLUGIN_EXIT() {
    g_workspaceHistory.commit();
}
