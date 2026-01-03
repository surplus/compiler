// Initial state
window.State.clicks().is(0);
window.State.enters().is(0);
window.State.leaves().is(0);
window.State.inputs().is(0);
$('#click-btn').only().hasText('Clicks: 0');

// Click button multiple times
$('#click-btn').only().click();
window.State.clicks().is(1);
$('#click-btn').only().hasText('Clicks: 1');

$('#click-btn').only().click();
window.State.clicks().is(2);
$('#click-btn').only().hasText('Clicks: 2');

$('#click-btn').only().click();
window.State.clicks().is(3);

// Hover events
const hoverDiv = $('#hover-div').only();
hoverDiv.dispatchEvent(new MouseEvent('mouseenter'));
window.State.enters().is(1);

hoverDiv.dispatchEvent(new MouseEvent('mouseleave'));
window.State.leaves().is(1);

hoverDiv.dispatchEvent(new MouseEvent('mouseenter'));
window.State.enters().is(2);

// Input event
const input = $('#text-input').only();
input.value = 'test';
input.dispatchEvent(new Event('input'));
window.State.inputs().is(1);
window.State.value().is('test');

input.value = 'hello';
input.dispatchEvent(new Event('input'));
window.State.inputs().is(2);
window.State.value().is('hello');
