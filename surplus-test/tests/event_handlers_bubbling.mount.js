// Initial state
window.State.parentClicks().is(0);
window.State.childClicks().is(0);

// Click child - should bubble to parent
$('#child').only().click();
window.State.childClicks().is(1);
window.State.parentClicks().is(1);

// Click parent directly
$('#parent').only().click();
window.State.childClicks().is(1); // unchanged
window.State.parentClicks().is(2);

// Enable stopPropagation
window.State.stopProp(true);

// Click child - should NOT bubble to parent now
$('#child').only().click();
window.State.childClicks().is(2);
window.State.parentClicks().is(2); // unchanged

// Disable stopPropagation
window.State.stopProp(false);

// Click child - should bubble again
$('#child').only().click();
window.State.childClicks().is(3);
window.State.parentClicks().is(3);
