// Initial state
$('div').only().exactProps({
	id: 'elem-1',
	class: 'box',
	'data-value': '42',
	title: 'Hover me'
});

// Change id
window.State.id('elem-2');
$('div').only().exactProps({
	id: 'elem-2',
	class: 'box',
	'data-value': '42',
	title: 'Hover me'
});

// Change class
window.State.className('container');
$('div').only().exactProps({
	id: 'elem-2',
	class: 'container',
	'data-value': '42',
	title: 'Hover me'
});

// Change data attribute
window.State.dataValue('100');
$('div').only().exactProps({
	id: 'elem-2',
	class: 'container',
	'data-value': '100',
	title: 'Hover me'
});

// Change title
window.State.title('New title');
$('div').only().exactProps({
	id: 'elem-2',
	class: 'container',
	'data-value': '100',
	title: 'New title'
});

// Element should be the same instance throughout
$('*').length.is(1);
