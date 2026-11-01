#include "FocusLayout.hpp"
#include "FocusStrip.hpp"
#include <algorithm>
#include <charconv>
#include <hyprland/src/config/ConfigValue.hpp>
#include <hyprland/src/desktop/history/WindowHistoryTracker.hpp>
#include <hyprland/src/desktop/state/FocusState.hpp>
#include <hyprland/src/desktop/view/window/Window.hpp>
#include <hyprland/src/desktop/view/window/WindowPresentation.hpp>
#include <hyprland/src/event/EventBus.hpp>
#include <hyprland/src/layout/algorithm/Algorithm.hpp>
#include <hyprland/src/layout/space/Space.hpp>
#include <hyprland/src/layout/target/Target.hpp>
#include <hyprland/src/managers/fullscreen/FullscreenController.hpp>
#include <hyprland/src/managers/fullscreen/handler/FullscreenHandler.hpp>
#include <hyprland/src/output/Monitor.hpp>
#include <hyprland/src/plugins/PluginAPI.hpp>
#include <hyprland/src/state/MonitorState.hpp>

extern HANDLE g_handle;
using namespace Layout;
using namespace Desktop;
using namespace Desktop::View;

CFocusLayout::CFocusLayout() {
    m_focusListener = Event::bus()->m_events.window.active.listen([this](PHLWINDOW window, eFocusReason) {
        if (window && contains(window->layoutTarget()))
            select(window->layoutTarget());
    });
}

CFocusLayout::~CFocusLayout() {
    m_recalculating = true;
    m_focusListener.reset();
    for (auto &entry : m_entries)
        release(entry);
}

bool CFocusLayout::contains(SP<ITarget> target) const {
    return std::ranges::any_of(m_entries, [&target](const auto &e) { return e.target == target; });
}

void CFocusLayout::newTarget(SP<ITarget> target) {
    RASSERT(target->type() == TARGET_TYPE_WINDOW, "Focus layout received a group target");
    if (contains(target))
        return;
    auto strip = makeShared<CFocusStrip>(target->window(), *this);
    m_entries.push_back({target, strip});
    HyprlandAPI::addWindowDecoration(g_handle, target->window(), std::move(strip));
    const auto monitor = focusState()->monitor();
    const auto workspace = m_parent->space()->workspace();
    const bool focusedHere = focusState()->window() == target->window() && monitor &&
                             (monitor->m_activeSpecialWorkspace ? monitor->m_activeSpecialWorkspace == workspace : monitor->m_activeWorkspace == workspace);
    if (!m_selected || focusedHere)
        m_selected = target;
    recalculate();
}

void CFocusLayout::movedTarget(SP<ITarget> target, std::optional<Vector2D>) { newTarget(target); }

void CFocusLayout::release(Entry &entry) {
    if (entry.strip) {
        entry.strip->detach();
        HyprlandAPI::removeWindowDecoration(g_handle, entry.strip.get());
    }
    if (entry.target && entry.target->window()) {
        auto window = entry.target->window();
        window->setInputBlocked(FOCUS_BLOCK_MONOCLE_INACTIVE, false);
        window->presentation().alpha(WINDOW_ALPHA_LAYOUT)->setValueAndWarp(1.F);
    }
}

void CFocusLayout::removeTarget(SP<ITarget> target) {
    const auto it = std::ranges::find_if(m_entries, [&target](const auto &e) { return e.target == target; });
    if (it == m_entries.end())
        return;
    const size_t index = std::distance(m_entries.begin(), it);
    auto removed = std::move(*it);
    m_entries.erase(it);
    if (m_selected == target) {
        m_selected.reset();
        if (!m_entries.empty()) {
            for (const auto &window : History::windowTracker()->historyForWorkspace(m_parent->space()->workspace()) | std::views::reverse) {
                if (window && contains(window->layoutTarget())) {
                    m_selected = window->layoutTarget();
                    break;
                }
            }
            if (!m_selected)
                m_selected = m_entries[std::min(index, m_entries.size() - 1)].target;
        }
    }
    release(removed);
    recalculate();
}

void CFocusLayout::select(SP<ITarget> target) {
    if (m_selected == target)
        return;
    m_selected = target;
    recalculate();
}

void CFocusLayout::recalculate(eRecalculateReason) {
    if (m_recalculating || !m_parent || !m_parent->space())
        return;
    m_recalculating = true;
    for (auto &entry : m_entries) {
        if (!entry.target || !entry.target->window())
            continue;
        auto window = entry.target->window();
        const bool selected = entry.target == m_selected;
        window->setInputBlocked(FOCUS_BLOCK_MONOCLE_INACTIVE, !selected);
        window->presentation().alpha(WINDOW_ALPHA_LAYOUT)->setValueAndWarp(selected ? 1.F : 0.F);
        g_pDecorationPositioner->repositionDeco(entry.strip.get());
    }
    auto space = m_parent->space();
    space->recheckWorkArea();
    if (Fullscreen::controller()->hasFullscreen(space->workspace(), true))
        m_defaultFullscreenHandler->syncTargetSizeAndPosition();
    else {
        for (const auto &entry : m_entries) {
            if (entry.target)
                entry.target->setPositionGlobal(space->workArea());
        }
    }
    m_recalculating = false;
}

std::vector<PHLWINDOW> CFocusLayout::windows() const {
    std::vector<PHLWINDOW> result;
    for (const auto &entry : m_entries) {
        if (entry.target && entry.target->window())
            result.push_back(entry.target->window());
    }
    return result;
}

PHLWINDOW CFocusLayout::selectedWindow() const { return m_selected ? m_selected->window() : nullptr; }

void CFocusLayout::focus(size_t index) {
    if (index >= m_entries.size() || !m_entries[index].target)
        return;
    const auto target = m_entries[index].target.lock();
    select(target);
    focusState()->fullWindowFocus(target->window(), FOCUS_REASON_KEYBIND);
}

void CFocusLayout::cycle(int delta) {
    if (m_entries.empty())
        return;
    const auto it = std::ranges::find_if(m_entries, [this](const auto &e) { return e.target == m_selected; });
    const auto index = std::distance(m_entries.begin(), it);
    focus((index + delta + m_entries.size()) % m_entries.size());
}

SP<ITarget> CFocusLayout::getNextCandidate(SP<ITarget> old) {
    if (m_selected && m_selected != old)
        return m_selected.lock();
    for (const auto &window : History::windowTracker()->historyForWorkspace(m_parent->space()->workspace()) | std::views::reverse) {
        if (window && window->layoutTarget() != old && contains(window->layoutTarget()))
            return window->layoutTarget();
    }
    for (const auto &entry : m_entries) {
        if (entry.target && entry.target != old)
            return entry.target.lock();
    }
    return nullptr;
}

std::optional<Vector2D> CFocusLayout::predictSizeForNewTarget() { return m_parent->space()->workArea().size(); }

void CFocusLayout::swapTargets(SP<ITarget> a, SP<ITarget> b) {
    auto ia = std::ranges::find_if(m_entries, [&a](const auto &e) { return e.target == a; });
    auto ib = std::ranges::find_if(m_entries, [&b](const auto &e) { return e.target == b; });
    if (ia != m_entries.end() && ib != m_entries.end())
        std::iter_swap(ia, ib);
    else if (ia != m_entries.end()) {
        const auto index = std::distance(m_entries.begin(), ia);
        removeTarget(a);
        newTarget(b);
        std::rotate(m_entries.begin() + index, m_entries.end() - 1, m_entries.end());
    }
    recalculate();
}

void CFocusLayout::moveTargetInDirection(SP<ITarget> target, Math::eDirection direction, bool) {
    static auto fallback = CConfigValue<Config::INTEGER>("binds:window_direction_monitor_fallback");
    if (!*fallback || !target->space())
        return;
    const auto monitor = State::monitorState()->query().relativeTo(target->workspace()->m_monitor.lock()).inDirection(direction).run();
    if (monitor && monitor != target->workspace()->m_monitor)
        target->assignToSpace(monitor->m_activeWorkspace->space(), focalPointForDir(target, direction));
}

Config::ErrorResult CFocusLayout::layoutMsg(const std::string_view &message) {
    if (message == "cyclenext")
        cycle(1);
    else if (message == "cycleprev")
        cycle(-1);
    else if (message == "movetab 1" || message == "movetab -1") {
        if (m_entries.size() > 1) {
            auto it = std::ranges::find_if(m_entries, [this](const auto &e) { return e.target == m_selected; });
            const auto index = std::distance(m_entries.begin(), it);
            const auto next = (index + (message == "movetab 1" ? 1 : -1) + m_entries.size()) % m_entries.size();
            std::iter_swap(it, m_entries.begin() + next);
            recalculate();
        }
    } else if (message.starts_with("focustab ")) {
        unsigned index = 0;
        auto text = message.substr(9);
        auto [end, error] = std::from_chars(text.data(), text.data() + text.size(), index);
        if (error != std::errc{} || end != text.data() + text.size() || index == 0 || index > m_entries.size())
            return Config::configError("Invalid tab index");
        focus(index - 1);
    } else
        return Config::configError("Unknown focus layout command");
    return {};
}
