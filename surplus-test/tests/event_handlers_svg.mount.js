// Verify SVG structure
$('svg').only();
$('circle').only().isTag('circle');
$('rect').only().isTag('rect');

// Initial state
window.State.circleClicks().is(0);
window.State.rectClicks().is(0);

// Click circle
$('#test-circle').only().click();
window.State.circleClicks().is(1);
window.State.rectClicks().is(0);

// Click rect
$('#test-rect').only().click();
window.State.circleClicks().is(1);
window.State.rectClicks().is(1);

// Click circle again
$('#test-circle').only().click();
window.State.circleClicks().is(2);
window.State.rectClicks().is(1);
