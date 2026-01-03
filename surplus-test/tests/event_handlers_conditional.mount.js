// Initial state: button exists
$('#conditional-btn').only();
window.State.count().is(0);

// Click button
$('#conditional-btn').only().click();
window.State.count().is(1);
$('#conditional-btn').only().hasText('Count: 1');

// Hide button
window.State.show(false);
$('#conditional-btn').length.is(0);
$('#container').only().text().is('');

// Show button again
window.State.show(true);
$('#conditional-btn').only();
$('#conditional-btn').only().hasText('Count: 1');

// Click again
$('#conditional-btn').only().click();
window.State.count().is(2);
$('#conditional-btn').only().hasText('Count: 2');

// Toggle a few more times
window.State.show(false);
$('#conditional-btn').length.is(0);

window.State.show(true);
$('#conditional-btn').only().click();
window.State.count().is(3);
