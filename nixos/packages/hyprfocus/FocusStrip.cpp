#include "FocusStrip.hpp"
#include "FocusLayout.hpp"
#include <algorithm>
#include <hyprland/src/config/ConfigValue.hpp>
#include <hyprland/src/desktop/view/window/Window.hpp>
#include <hyprland/src/desktop/view/window/WindowMetadata.hpp>
#include <hyprland/src/desktop/view/window/WindowPresentation.hpp>
#include <hyprland/src/devices/IPointer.hpp>
#include <hyprland/src/output/Monitor.hpp>
#include <hyprland/src/render/Renderer.hpp>
#include <hyprland/src/render/WindowRenderPresentation.hpp>
#include <hyprland/src/render/pass/RectPassElement.hpp>
#include <hyprland/src/render/pass/TexPassElement.hpp>

using namespace Render;

CFocusStrip::CFocusStrip(PHLWINDOW window, CFocusLayout &layout) : IHyprWindowDecoration(window), m_window(window), m_layout(&layout) {}

void CFocusStrip::detach() {
    damageEntire();
    m_layout = nullptr;
}

SDecorationPositioningInfo CFocusStrip::getPositioningInfo() {
    static auto height = CConfigValue<Config::INTEGER>("group:groupbar:height");
    SDecorationPositioningInfo info;
    info.policy = DECORATION_POSITION_STICKY;
    info.edges = DECORATION_EDGE_TOP;
    info.priority = 3;
    info.reserved = true;
    const bool visible = m_layout && m_layout->size() > 1;
    info.desiredExtents = {{0, visible ? static_cast<int>(std::max<int64_t>(*height, 20)) : 0}, {0, 0}};
    return info;
}

void CFocusStrip::onPositioningReply(const SDecorationPositioningReply &reply) { m_box = reply.assignedGeometry; }

CBox CFocusStrip::globalBox() const {
    auto box = m_box;
    if (m_window) {
        box.translate(g_pDecorationPositioner->getEdgeDefinedPoint(DECORATION_EDGE_TOP, m_window));
        box.translate(m_window->presentation().renderPresentation().workspaceOffset);
    }
    return box;
}

void CFocusStrip::damageEntire() {
    if (m_window)
        g_pHyprRenderer->damageBox(globalBox());
}

void CFocusStrip::updateWindow(PHLWINDOW) { damageEntire(); }

void CFocusStrip::draw(CRenderContext &ctx, PHLMONITOR monitor, const float &alpha, const SWindowRenderPresentation &presentation) {
    if (!m_layout || !m_window)
        return;
    const auto windows = m_layout->windows();
    if (windows.size() < 2)
        return;
    static auto active = CConfigValue<Config::IComplexConfigValue>("group:groupbar:col.active");
    static auto inactive = CConfigValue<Config::IComplexConfigValue>("group:groupbar:col.inactive");
    static auto fontSize = CConfigValue<Config::INTEGER>("group:groupbar:font_size");
    static auto fontFamily = CConfigValue<std::string>("group:groupbar:font_family");
    static auto fallbackFont = CConfigValue<std::string>("misc:font_family");
    static auto textActive = CConfigValue<Config::INTEGER>("group:groupbar:text_color");
    static auto textInactive = CConfigValue<Config::INTEGER>("group:groupbar:text_color_inactive");
    const auto font = *fontFamily == STRVAL_EMPTY ? *fallbackFont : *fontFamily;
    auto box = m_box;
    box.translate(g_pDecorationPositioner->getEdgeDefinedPoint(DECORATION_EDGE_TOP, m_window));
    box.translate(presentation.workspaceOffset + presentation.floatingOffset - monitor->m_position);
    box.scale(monitor->m_scale);
    if (box.empty())
        return;
    const double width = box.w / windows.size();
    m_text.resize(windows.size());
    for (size_t i = 0; i < windows.size(); ++i) {
        CBox tab{box.x + i * width, box.y, width - 1, box.h};
        auto color = static_cast<Config::CGradientValueData *>((windows[i] == m_layout->selectedWindow() ? active.ptr() : inactive.ptr()))->m_colors[0];
        color.a *= alpha;
        CRectPassElement::SRectData rect;
        rect.box = tab;
        rect.color = color;
        rect.workspacePresentation = presentation;
        g_pHyprRenderer->addPassElement(ctx, makeUnique<CRectPassElement>(rect));
        const int textWidth = std::max(0, static_cast<int>(width - 12 * monitor->m_scale));
        if (!textWidth)
            continue;
        auto &text = m_text[i];
        const auto title = windows[i]->metadata().title();
        const auto textColor = windows[i] == m_layout->selectedWindow() || *textInactive == -1 ? *textActive : *textInactive;
        if (!text.texture || text.title != title || text.width != textWidth || text.scale != monitor->m_scale || text.font != font ||
            text.fontSize != *fontSize || text.color != textColor) {
            text = {title,
                    textWidth,
                    monitor->m_scale,
                    font,
                    *fontSize,
                    textColor,
                    g_pHyprRenderer->renderText(title, CHyprColor{static_cast<uint64_t>(textColor)}, *fontSize * monitor->m_scale, false, font, textWidth)};
        }
        if (!text.texture)
            continue;
        CTexPassElement::SRenderData data;
        data.tex = text.texture;
        data.box = CBox{tab.pos() + (tab.size() - text.texture->m_size) / 2, text.texture->m_size};
        data.a = alpha;
        data.workspacePresentation = presentation;
        g_pHyprRenderer->addPassElement(ctx, makeUnique<CTexPassElement>(std::move(data)));
    }
}

bool CFocusStrip::onInputOnDeco(eInputType type, const Vector2D &position, std::any data) {
    if (!m_layout || m_layout->size() < 2)
        return false;
    if (type == INPUT_TYPE_AXIS) {
        const auto event = std::any_cast<IPointer::SAxisEvent>(data);
        if (event.delta != 0)
            m_layout->cycle(event.delta > 0 ? 1 : -1);
        return true;
    }
    if (type != INPUT_TYPE_BUTTON)
        return false;
    const auto event = std::any_cast<IPointer::SButtonEvent>(data);
    if (event.button != 272)
        return false;
    if (event.state == WL_POINTER_BUTTON_STATE_PRESSED) {
        const auto box = globalBox();
        if (box.w > 0)
            m_layout->focus(std::clamp<int>((position.x - box.x) / box.w * m_layout->size(), 0, m_layout->size() - 1));
    }
    return true;
}
