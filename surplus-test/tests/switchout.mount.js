$('*').length.is(2);
$('div').only().text().is('Current child: Default');
$('span').only().text().is('Default');

window.State.show('A');
$('div').only().text().is('Current child: A');
$('span').only().text().is('A');

window.State.show('B');
$('div').only().text().is('Current child: B');
$('span').only().text().is('B');

window.State.show(null);
$('div').only().text().is('Current child: Default');
$('span').only().text().is('Default');

window.State.show('C');
$('div').only().text().is('Current child: Default');
$('span').only().text().is('Default');
