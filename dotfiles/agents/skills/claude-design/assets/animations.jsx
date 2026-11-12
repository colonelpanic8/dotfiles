/*
  animations.jsx — Stage + Sprite timeline primitives for motion design.

  Attach as a <script type="text/babel" src="animations.jsx"></script>, then use:

    const { Stage, Sprite, useTime, useSprite, Easing, interpolate } = window.Anim;

    const Intro = () => {
      const t = useSprite();          // 0..1 within this Sprite's active window
      const opacity = Easing.outCubic(t);
      return <div style={{ opacity }}>Hello</div>;
    };

    ReactDOM.render(
      <Stage duration={6.0} width={1920} height={1080}>
        <Sprite start={0} end={2}><Intro /></Sprite>
        <Sprite start={1.5} end={4}><Middle /></Sprite>
        <Sprite start={3.5} end={6}><Outro /></Sprite>
      </Stage>,
      document.getElementById('root')
    );

  Features:
  - Scales 1920×1080 to viewport, letterboxed
  - Play / pause / scrub controls outside the scaled canvas
  - localStorage persistence of playhead for iteration workflow
  - useTime() returns stage time in seconds; useSprite() returns 0..1 within the sprite
  - interpolate(t, [t0, t1], [v0, v1], easing?) for keyframes
  - Easing library: linear, inQuad, outQuad, inOutCubic, outCubic, inOutSine, etc.
*/

const StageContext = React.createContext({ time: 0, duration: 0 });
const SpriteContext = React.createContext({ start: 0, end: 0 });

const Easing = {
  linear: (t) => t,
  inQuad: (t) => t * t,
  outQuad: (t) => 1 - (1 - t) * (1 - t),
  inOutQuad: (t) => (t < 0.5 ? 2 * t * t : 1 - ((-2 * t + 2) ** 2) / 2),
  inCubic: (t) => t * t * t,
  outCubic: (t) => 1 - (1 - t) ** 3,
  inOutCubic: (t) => (t < 0.5 ? 4 * t * t * t : 1 - ((-2 * t + 2) ** 3) / 2),
  inOutSine: (t) => -(Math.cos(Math.PI * t) - 1) / 2,
};

function interpolate(t, input, output, easing = Easing.linear) {
  const [i0, i1] = input;
  const [o0, o1] = output;
  if (t <= i0) return o0;
  if (t >= i1) return o1;
  const p = easing((t - i0) / (i1 - i0));
  return o0 + (o1 - o0) * p;
}

const useTime = () => React.useContext(StageContext).time;

const useSprite = () => {
  const { time } = React.useContext(StageContext);
  const { start, end } = React.useContext(SpriteContext);
  if (end <= start) return 0;
  return Math.max(0, Math.min(1, (time - start) / (end - start)));
};

const Sprite = ({ start, end, children }) => {
  const { time } = React.useContext(StageContext);
  const visible = time >= start && time <= end;
  if (!visible) return null;
  return (
    <SpriteContext.Provider value={{ start, end }}>
      {children}
    </SpriteContext.Provider>
  );
};

const Stage = ({ duration = 6, width = 1920, height = 1080, storageKey = 'anim-time', children }) => {
  const [time, setTime] = React.useState(() => {
    const stored = Number(localStorage.getItem(storageKey));
    return Number.isFinite(stored) ? Math.min(duration, Math.max(0, stored)) : 0;
  });
  const [playing, setPlaying] = React.useState(false);
  const [scale, setScale] = React.useState(1);
  const rafRef = React.useRef(null);
  const lastRef = React.useRef(null);

  // Scale the canvas to fit the viewport, letterboxed
  React.useEffect(() => {
    const fit = () => {
      const sx = window.innerWidth / width;
      const sy = window.innerHeight / height;
      setScale(Math.min(sx, sy));
    };
    fit();
    window.addEventListener('resize', fit);
    return () => window.removeEventListener('resize', fit);
  }, [width, height]);

  // Animation frame loop
  React.useEffect(() => {
    if (!playing) {
      if (rafRef.current) cancelAnimationFrame(rafRef.current);
      lastRef.current = null;
      return;
    }
    const tick = (now) => {
      if (lastRef.current == null) lastRef.current = now;
      const dt = (now - lastRef.current) / 1000;
      lastRef.current = now;
      setTime((t) => {
        const next = t + dt;
        if (next >= duration) { setPlaying(false); return duration; }
        return next;
      });
      rafRef.current = requestAnimationFrame(tick);
    };
    rafRef.current = requestAnimationFrame(tick);
    return () => { if (rafRef.current) cancelAnimationFrame(rafRef.current); };
  }, [playing, duration]);

  // Persist playhead
  React.useEffect(() => {
    localStorage.setItem(storageKey, String(time));
  }, [time, storageKey]);

  return (
    <StageContext.Provider value={{ time, duration }}>
      <div style={{ position: 'fixed', inset: 0, background: '#000', display: 'grid', placeItems: 'center' }}>
        <div style={{
          width, height, transform: `scale(${scale})`, transformOrigin: 'center',
          background: 'white', position: 'relative', overflow: 'hidden',
        }}>
          {children}
        </div>
      </div>

      <div style={{
        position: 'fixed', bottom: 16, left: '50%', transform: 'translateX(-50%)',
        display: 'flex', gap: 12, alignItems: 'center',
        background: 'rgba(0,0,0,0.7)', backdropFilter: 'blur(8px)',
        padding: '10px 16px', borderRadius: 8, color: 'white', fontSize: 13,
      }}>
        <button
          onClick={() => {
            if (time >= duration) { setTime(0); setPlaying(true); }
            else setPlaying((p) => !p);
          }}
          style={{ background: 'rgba(255,255,255,0.15)', border: 'none', color: 'white', padding: '6px 12px', borderRadius: 4, cursor: 'pointer' }}
        >
          {playing ? 'Pause' : (time >= duration ? 'Replay' : 'Play')}
        </button>
        <input
          type="range"
          min={0} max={duration} step={0.01}
          value={time}
          onChange={(e) => { setPlaying(false); setTime(Number(e.target.value)); }}
          style={{ width: 400 }}
        />
        <span style={{ fontVariantNumeric: 'tabular-nums', minWidth: 80, textAlign: 'right' }}>
          {time.toFixed(2)}s / {duration.toFixed(2)}s
        </span>
      </div>
    </StageContext.Provider>
  );
};

window.Anim = { Stage, Sprite, useTime, useSprite, Easing, interpolate };
