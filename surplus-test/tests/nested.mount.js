// 5 divs + 1 span = 6 elements total
$('*').length.is(6);
$('div').length.is(5);

// Check each level exists and is nested correctly
$('#level-1').only();
$('#level-1 > #level-2').only();
$('#level-2 > #level-3').only();
$('#level-3 > #level-4').only();
$('#level-4 > #level-5').only();
$('#level-5 > span').only().text().is('Deep');

// Verify deepest element has correct ancestry
$('#level-1 #level-5 span').only().text().is('Deep');
