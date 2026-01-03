// div + strong + br + span = 4 elements
$('*').length.is(4);

// Check normalized text content matches expected output
$('div').only().text().isLike(`
	Hello World,
	you have 42 messages.
	Your score: 84 (doubled)
`);

$('strong').only().text().is('World');
$('span').only().text().is(' (doubled)');
$('br').length.is(1);
