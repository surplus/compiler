// Initial state
window.State.handler1().is(0);
window.State.handler2().is(0);
window.State.handler3().is(0);

// Click once - all handlers should fire
$('#multi-handler').only().click();
window.State.handler1().is(1);
window.State.handler2().is(1);
window.State.handler3().is(10);

// Click again
$('#multi-handler').only().click();
window.State.handler1().is(2);
window.State.handler2().is(2);
window.State.handler3().is(20);
