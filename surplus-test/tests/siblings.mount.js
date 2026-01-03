// Root div + 3 p + 2 span + 1 div + 3 h = 10 elements
$('*').length.is(10);

// Check direct children of root
$('body > div > *').length.is(6);

// Three paragraphs
$('p').length.is(3);
$('p')[0].text().is('First');
$('p')[1].text().is('Second');
$('p')[2].text().is('Third');

// Two spans
$('span').length.is(2);
$('span')[0].text().is('Fourth');
$('span')[1].text().is('Fifth');

// Nested headings
$('body > div > div').only();
$('body > div > div > *').length.is(3);
$('h1').only().text().is('Nested First');
$('h2').only().text().is('Nested Second');
$('h3').only().text().is('Nested Third');
