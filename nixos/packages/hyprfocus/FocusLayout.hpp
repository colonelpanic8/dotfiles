#pragma once

#include <hyprland/src/desktop/DesktopTypes.hpp>
#include <hyprland/src/helpers/signal/Signal.hpp>
#include <hyprland/src/layout/algorithm/TiledAlgorithm.hpp>
#include <vector>

class CFocusStrip;

class CFocusLayout final : public Layout::ITiledAlgorithm {
  public:
    CFocusLayout();
    ~CFocusLayout() override;
    bool acceptsGroupTargets() const override { return false; }
    void newTarget(SP<Layout::ITarget>) override;
    void movedTarget(SP<Layout::ITarget>, std::optional<Vector2D>) override;
    void removeTarget(SP<Layout::ITarget>) override;
    void resizeTarget(const Vector2D &, SP<Layout::ITarget>, Layout::eRectCorner) override {}
    void recalculate(Layout::eRecalculateReason = Layout::RECALCULATE_REASON_UNKNOWN) override;
    void swapTargets(SP<Layout::ITarget>, SP<Layout::ITarget>) override;
    void moveTargetInDirection(SP<Layout::ITarget>, Math::eDirection, bool) override;
    SP<Layout::ITarget> getNextCandidate(SP<Layout::ITarget>) override;
    Config::ErrorResult layoutMsg(const std::string_view &) override;
    std::optional<Vector2D> predictSizeForNewTarget() override;

    size_t size() const { return m_entries.size(); }
    std::vector<PHLWINDOW> windows() const;
    PHLWINDOW selectedWindow() const;
    void focus(size_t index);
    void cycle(int delta);

  private:
    struct Entry {
        WP<Layout::ITarget> target;
        SP<CFocusStrip> strip;
    };
    std::vector<Entry> m_entries;
    WP<Layout::ITarget> m_selected;
    CHyprSignalListener m_focusListener;
    bool m_recalculating = false;
    bool contains(SP<Layout::ITarget>) const;
    void select(SP<Layout::ITarget>);
    void release(Entry &);
};
