# Device frames — ready-to-paste CSS shells

When a design mocks a mobile or desktop app, wrap it in a device frame so it reads as the real thing, not a floating panel. Don't SVG-draw elaborate bezels — the four frames below cover 99% of cases, and adding more detail (speaker grills, bevels, reflection shine) usually doesn't add value.

## iOS frame (iPhone 15-era, 393×852)

```html
<style>
  .ios-frame {
    width: 393px; height: 852px;
    background: #000;
    border-radius: 56px;
    padding: 14px;
    box-shadow: 0 1px 0 rgba(255,255,255,0.1) inset, 0 30px 60px rgba(0,0,0,0.25);
    position: relative;
  }
  .ios-frame-screen {
    width: 100%; height: 100%;
    background: white;
    border-radius: 44px;
    overflow: hidden;
    position: relative;
  }
  .ios-dynamic-island {
    position: absolute; top: 11px; left: 50%; transform: translateX(-50%);
    width: 126px; height: 37px;
    background: #000; border-radius: 20px;
    z-index: 10;
  }
  .ios-status-bar {
    position: absolute; top: 0; left: 0; right: 0;
    height: 54px; padding: 0 30px;
    display: flex; align-items: center; justify-content: space-between;
    font: 600 17px/1 -apple-system, BlinkMacSystemFont, sans-serif;
    z-index: 5;
  }
  .ios-home-indicator {
    position: absolute; bottom: 8px; left: 50%; transform: translateX(-50%);
    width: 140px; height: 5px;
    background: #000; border-radius: 3px;
    z-index: 10;
  }
</style>

<div class="ios-frame">
  <div class="ios-frame-screen">
    <div class="ios-dynamic-island"></div>
    <div class="ios-status-bar">
      <span>9:41</span>
      <span>●●● ◉ ▬</span>
    </div>
    <!-- Your app content goes here, padded to avoid overlap with status bar / home indicator -->
    <main style="padding: 60px 20px 40px;">
      <!-- app -->
    </main>
    <div class="ios-home-indicator"></div>
  </div>
</div>
```

## Android frame (Pixel-era, 412×915)

```html
<style>
  .android-frame {
    width: 412px; height: 915px;
    background: #111;
    border-radius: 48px;
    padding: 10px;
    box-shadow: 0 30px 60px rgba(0,0,0,0.25);
    position: relative;
  }
  .android-frame-screen {
    width: 100%; height: 100%;
    background: white;
    border-radius: 40px;
    overflow: hidden;
    position: relative;
  }
  .android-camera-cutout {
    position: absolute; top: 12px; left: 50%; transform: translateX(-50%);
    width: 12px; height: 12px;
    background: #000; border-radius: 50%;
    z-index: 10;
  }
  .android-status-bar {
    position: absolute; top: 0; left: 0; right: 0;
    height: 32px; padding: 0 20px;
    display: flex; align-items: center; justify-content: space-between;
    font: 500 13px/1 'Roboto', sans-serif;
    z-index: 5;
  }
  .android-nav-bar {
    position: absolute; bottom: 0; left: 0; right: 0;
    height: 28px;
    display: flex; align-items: center; justify-content: center;
    background: white;
    z-index: 10;
  }
  .android-nav-bar::after {
    content: '';
    width: 120px; height: 4px;
    background: #333; border-radius: 2px;
  }
</style>

<div class="android-frame">
  <div class="android-frame-screen">
    <div class="android-camera-cutout"></div>
    <div class="android-status-bar">
      <span>9:41</span>
      <span>▬▬▬ ◉</span>
    </div>
    <main style="padding: 44px 16px 40px;">
      <!-- app -->
    </main>
    <div class="android-nav-bar"></div>
  </div>
</div>
```

## macOS window frame

```html
<style>
  .macos-window {
    width: 1200px; min-height: 800px;
    background: white;
    border-radius: 10px;
    box-shadow: 0 30px 60px rgba(0,0,0,0.15), 0 0 0 1px rgba(0,0,0,0.08);
    overflow: hidden;
    position: relative;
  }
  .macos-titlebar {
    height: 38px;
    background: #f6f6f6;
    border-bottom: 1px solid #e5e5e5;
    display: flex; align-items: center; padding: 0 14px;
    gap: 8px;
  }
  .macos-titlebar .lights {
    display: flex; gap: 8px;
  }
  .macos-titlebar .light {
    width: 12px; height: 12px; border-radius: 50%;
  }
  .light.close { background: #ff5f57; }
  .light.min { background: #febc2e; }
  .light.max { background: #28c840; }
  .macos-title {
    flex: 1; text-align: center;
    font: 500 13px/1 -apple-system, BlinkMacSystemFont, sans-serif;
    color: #555;
  }
</style>

<div class="macos-window">
  <div class="macos-titlebar">
    <div class="lights">
      <span class="light close"></span>
      <span class="light min"></span>
      <span class="light max"></span>
    </div>
    <div class="macos-title">Your App</div>
    <div style="width: 52px;"><!-- balance the lights on the right --></div>
  </div>
  <main>
    <!-- app -->
  </main>
</div>
```

## Browser window frame

```html
<style>
  .browser-window {
    width: 1280px; min-height: 800px;
    background: white;
    border-radius: 10px;
    box-shadow: 0 30px 60px rgba(0,0,0,0.15), 0 0 0 1px rgba(0,0,0,0.08);
    overflow: hidden;
  }
  .browser-chrome {
    background: #f6f6f6;
    border-bottom: 1px solid #e5e5e5;
    padding: 10px 14px;
  }
  .browser-chrome .lights {
    display: flex; gap: 8px; margin-bottom: 10px;
  }
  .browser-chrome .light {
    width: 12px; height: 12px; border-radius: 50%;
  }
  .browser-chrome .url-bar {
    background: white;
    border: 1px solid #e5e5e5;
    border-radius: 20px;
    padding: 6px 16px;
    font: 500 13px/1 -apple-system, BlinkMacSystemFont, sans-serif;
    color: #555;
  }
</style>

<div class="browser-window">
  <div class="browser-chrome">
    <div class="lights">
      <span class="light" style="background:#ff5f57;"></span>
      <span class="light" style="background:#febc2e;"></span>
      <span class="light" style="background:#28c840;"></span>
    </div>
    <div class="url-bar">yoursite.com</div>
  </div>
  <main>
    <!-- site content -->
  </main>
</div>
```

## Usage rules

- Use the frame only for mocking a real device/app. A frame around a generic card is decoration, not craft.
- Keep the frame minimal — don't add battery percentages, signal bars, or extra chrome detail unless the design is about those elements.
- Content inside the frame must respect the safe area (status bar top, home indicator bottom on iOS; nav bar on Android; title bar on desktop).
- For interactive prototypes where the user clicks "home" / "back", treat the device frame as decoration — the real interaction lives in the content.

If you need a frame for a tablet, TV, smartwatch, or foldable — ask the user for one reference screenshot so you don't invent proportions that look slightly wrong.
