// Initial: mode 'a'
$('*').length.is(2); // div + span
$('#mode-a').only().text().is('Mode A');
$('#mode-b').length.is(0);

// Switch to mode 'b'
window.State.mode('b');
$('*').length.is(2); // div + span
$('#mode-a').length.is(0);
$('#mode-b').only().text().is('Mode B');

// Switch back to 'a'
window.State.mode('a');
$('*').length.is(2);
$('#mode-a').only().text().is('Mode A');
$('#mode-b').length.is(0);

// Switch to 'b' again
window.State.mode('b');
$('*').length.is(2);
$('#mode-a').length.is(0);
$('#mode-b').only().text().is('Mode B');
