#include "FocusLayout.hpp"
#include <hyprland/src/plugins/PluginAPI.hpp>
#include <stdexcept>

HANDLE g_handle = nullptr;
extern "C" int hyprland_focus_target_api_version();

APICALL EXPORT std::string PLUGIN_API_VERSION() { return HYPRLAND_API_VERSION; }

APICALL EXPORT PLUGIN_DESCRIPTION_INFO PLUGIN_INIT(HANDLE handle) {
    g_handle = handle;
    if (std::string_view{__hyprland_api_get_hash()} != __hyprland_api_get_client_hash() || hyprland_focus_target_api_version() != 1)
        throw std::runtime_error("hyprfocus requires the matching Hyprland focus-target patch");
    if (!HyprlandAPI::addTiledAlgo(handle, "tabbed", &typeid(CFocusLayout), [] { return makeUnique<CFocusLayout>(); }))
        throw std::runtime_error("Unable to register tabbed layout");
    return {"hyprfocus", "Workspace focus layout with independent window tabs", "Ivan Malison", "0.1"};
}

APICALL EXPORT void PLUGIN_EXIT() {}
