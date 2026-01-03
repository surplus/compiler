// Test event.target and event.currentTarget
$('#target-btn').only().click();
window.State.targetId().is('target-btn');
window.State.currentTargetId().is('target-btn');

// Test event.preventDefault
window.State.defaultPrevented().is(false);
$('#test-form').only().dispatchEvent(new Event('submit', { cancelable: true }));
window.State.defaultPrevented().is(true);

// Test keyboard event properties
const input = $('#key-input').only();
input.dispatchEvent(new KeyboardEvent('keydown', { key: 'a' }));
window.State.key().is('a');

input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
window.State.key().is('Enter');
