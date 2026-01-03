// Initial: show=false
$('*').length.is(2); // outer div + always div
$('#visible').length.is(0);
$('#always').only().text().is('Always here');

// Show it
window.State.show(true);
$('*').length.is(3); // outer div + span + always div
$('#visible').only().text().is('I am visible');
$('#always').only().text().is('Always here');

// Hide it
window.State.show(false);
$('*').length.is(2);
$('#visible').length.is(0);
$('#always').only().text().is('Always here');

// Show again
window.State.show(true);
$('*').length.is(3);
$('#visible').only().text().is('I am visible');
$('#always').only().text().is('Always here');
