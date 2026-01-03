// Should have 6 self-closing elements inside a div
$('*').length.is(7);

$('input').length.is(2);
$('input[type="text"]').only().exactProps({
	type: 'text',
	placeholder: 'Enter text'
});
$('input[type="checkbox"]').only().exactProps({
	type: 'checkbox',
	checked: ''
});

$('br').length.is(1);
$('img').only().exactProps({
	src: 'test.png',
	alt: 'Test image'
});
$('hr').length.is(1);
$('meta').only().exactProps({
	name: 'test',
	content: 'value'
});
