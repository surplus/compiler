// Initial state: multiplier = 1
window.State.count().is(0);
window.State.multiplier().is(1);

// Click - should increment by 1
$('#dynamic-btn').only().click();
window.State.count().is(1);

// Change multiplier to 10
window.State.multiplier(10);

// Click - should increment by 10
$('#dynamic-btn').only().click();
window.State.count().is(11);

// Click again
$('#dynamic-btn').only().click();
window.State.count().is(21);

// Change multiplier to 5
window.State.multiplier(5);

// Click - should increment by 5
$('#dynamic-btn').only().click();
window.State.count().is(26);

// Back to 1
window.State.multiplier(1);
$('#dynamic-btn').only().click();
window.State.count().is(27);
