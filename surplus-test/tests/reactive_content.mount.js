// Initial state: World, 42
$('*').length.is(4);
$('strong').only().text().is('World');
$('div').only().hasText('Hello World, you have 42 messages');
$('div').only().hasText('Your score: 84');

// Update name only
window.State.name('Alice');
$('strong').only().text().is('Alice');
$('div').only().hasText('Hello Alice, you have 42 messages');
$('div').only().hasText('Your score: 84');

// Update count only
window.State.count(10);
$('strong').only().text().is('Alice');
$('div').only().hasText('Hello Alice, you have 10 messages');
$('div').only().hasText('Your score: 20');

// Update both
window.State.name('Bob');
window.State.count(99);
$('strong').only().text().is('Bob');
$('div').only().hasText('Hello Bob, you have 99 messages');
$('div').only().hasText('Your score: 198');

// Back to original
window.State.name('World');
window.State.count(42);
$('strong').only().text().is('World');
$('div').only().hasText('Hello World, you have 42 messages');
$('div').only().hasText('Your score: 84');

// Element structure should remain constant throughout
$('*').length.is(4);
$('br').length.is(1);
$('span').only().text().is(' (doubled)');
