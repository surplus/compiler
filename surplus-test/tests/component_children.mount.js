// Structure: wrapper div + h1 + card div + 2 p + outer div + footer = 7 elements
$('*').length.is(7);

// Check wrapper
$('.wrapper').only();
$('.wrapper > h1').only().text().is('Title');
$('.wrapper > footer').only().text().is('Footer');

// Check card is child of wrapper
$('.wrapper > .card').only();

// Check paragraphs are inside card
$('.card > p').length.is(2);
$('.card > p')[0].text().is('Paragraph 1');
$('.card > p')[1].text().is('Paragraph 2');

// Check outside div
$('.wrapper > div').length.is(2); // card + outside
$('.wrapper > div:not(.card)').only().text().is('Outside card');
