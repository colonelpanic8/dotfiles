#pragma once
#include <hyprland/src/render/Texture.hpp>
#include <hyprland/src/render/decorations/IHyprWindowDecoration.hpp>
#include <string>
#include <vector>

class CFocusLayout;

class CFocusStrip final : public IHyprWindowDecoration {
  public:
    CFocusStrip(PHLWINDOW, CFocusLayout &);
    SDecorationPositioningInfo getPositioningInfo() override;
    void onPositioningReply(const SDecorationPositioningReply &) override;
    void draw(Render::CRenderContext &, PHLMONITOR, const float &, const Render::SWindowRenderPresentation &) override;
    eDecorationType getDecorationType() override { return DECORATION_CUSTOM; }
    void updateWindow(PHLWINDOW) override;
    void damageEntire() override;
    bool onInputOnDeco(eInputType, const Vector2D &, std::any) override;
    eDecorationLayer getDecorationLayer() override { return DECORATION_LAYER_OVER; }
    uint64_t getDecorationFlags() override { return DECORATION_ALLOWS_MOUSE_INPUT | DECORATION_PART_OF_MAIN_WINDOW; }
    std::string getDisplayName() override { return "Workspace tabs"; }
    void detach();

  private:
    struct Text {
        std::string title;
        int width = 0;
        float scale = 0;
        std::string font;
        int64_t fontSize = 0;
        int64_t color = 0;
        SP<Render::ITexture> texture;
    };
    PHLWINDOWREF m_window;
    CFocusLayout *m_layout;
    CBox m_box;
    std::vector<Text> m_text;
    CBox globalBox() const;
};
