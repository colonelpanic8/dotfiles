// Claude in Chrome (checked against v1.0.107) creates its tabs with
// chrome.tabs.create({url: "chrome://newtab", active: false}) and only groups
// them afterwards, so the group is not visible yet when onCreated fires.
// Its groups are titled "Claude (MCP)" or the session's display name.
const AGENT_GROUP_TITLE = /claude/i;

// Policy installs this for every user on the machine, so it stays off until
// someone turns it on; the setting follows their Google account.
async function isEnabled() {
  const {enabled = false} = await chrome.storage.sync.get("enabled");
  return enabled;
}

async function showState(enabled) {
  await chrome.action.setBadgeText({text: enabled ? "" : "off"});
  await chrome.action.setTitle({
    title: `New Tab, New Window: ${enabled ? "on" : "off"} (click to toggle)`,
  });
}

function isAgentCreatedTab(tab) {
  const url = tab.pendingUrl || tab.url || "";
  return !tab.active && url.startsWith("chrome://newtab");
}

async function isOpenedFromAgentTab(tab) {
  if (tab.openerTabId == null) return false;
  const opener = await chrome.tabs.get(tab.openerTabId).catch(() => null);
  if (!opener || opener.groupId === chrome.tabGroups.TAB_GROUP_ID_NONE) return false;
  const group = await chrome.tabGroups.get(opener.groupId).catch(() => null);
  return Boolean(group && AGENT_GROUP_TITLE.test(group.title || ""));
}

chrome.tabs.onCreated.addListener(async (tab) => {
  if (tab.index === 0) return;
  if (!(await isEnabled())) return;
  if (isAgentCreatedTab(tab)) return;
  if (await isOpenedFromAgentTab(tab)) return;

  const win = await chrome.windows.get(tab.windowId).catch(() => null);
  if (!win) return;
  await chrome.windows
    .create({tabId: tab.id, incognito: tab.incognito, state: win.state})
    .catch(() => {});
});

chrome.action.onClicked.addListener(async () => {
  await chrome.storage.sync.set({enabled: !(await isEnabled())});
});

chrome.storage.onChanged.addListener((changes, area) => {
  if (area === "sync" && "enabled" in changes) showState(Boolean(changes.enabled.newValue));
});

isEnabled().then(showState);
