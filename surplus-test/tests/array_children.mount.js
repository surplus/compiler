// Root div + ul + 3 li + numbers div + 5 span = 11 elements
$('*').length.is(11);

// Check list items
$('ul').only();
$('li').length.is(3);
$('li')[0].text().is('Apple');
$('li')[1].text().is('Banana');
$('li')[2].text().is('Cherry');

// Check number spans
$('.numbers').only();
$('.num').length.is(5);
$('.num')[0].text().is('2');
$('.num')[1].text().is('4');
$('.num')[2].text().is('6');
$('.num')[3].text().is('8');
$('.num')[4].text().is('10');
