// Initial state: not disabled, count is 0
$('#counter').only().checkProps({ id: 'counter' });
$('#counter').only().hasText('Count: 0');

// Click it - should increment
$('#counter').only().click();
$('#counter').only().hasText('Count: 1');

// Enable disabled
window.State.disabled(true);
$('#counter').only().checkProps({ id: 'counter', disabled: true });
$('#counter').only().hasText('Count: 1'); // Sameness check 1

// Try to click while disabled - count shouldn't change
$('#counter').only().click();
$('#counter').only().hasText('Count: 1'); // Sameness check 2

// Disable disabled
window.State.disabled(false);
$('#counter').only().checkProps({ id: 'counter' });
$('#counter').only().hasText('Count: 1'); // Sameness check 3

// Click again - should increment
$('#counter').only().click();
$('#counter').only().hasText('Count: 2');

// --- Round 2 ---
// Enable disabled again
window.State.disabled(true);
$('#counter').only().checkProps({ id: 'counter', disabled: true });
$('#counter').only().hasText('Count: 2'); // Sameness check 1

// Try clicking while disabled
$('#counter').only().click();
$('#counter').only().hasText('Count: 2'); // Sameness check 2

// Disable disabled
window.State.disabled(false);
$('#counter').only().checkProps({ id: 'counter' });
$('#counter').only().hasText('Count: 2'); // Sameness check 3

// Click - should increment
$('#counter').only().click();
$('#counter').only().hasText('Count: 3');

// --- Round 3 ---
// Enable disabled
window.State.disabled(true);
$('#counter').only().checkProps({ id: 'counter', disabled: true });
$('#counter').only().hasText('Count: 3'); // Sameness check 1

// Try clicking while disabled
$('#counter').only().click();
$('#counter').only().hasText('Count: 3'); // Sameness check 2

// Disable disabled
window.State.disabled(false);
$('#counter').only().checkProps({ id: 'counter' });
$('#counter').only().hasText('Count: 3'); // Sameness check 3

// Click - should increment
$('#counter').only().click();
$('#counter').only().hasText('Count: 4');

// --- Round 4 ---
// Enable disabled
window.State.disabled(true);
$('#counter').only().checkProps({ id: 'counter', disabled: true });
$('#counter').only().hasText('Count: 4'); // Sameness check 1

// Try clicking while disabled
$('#counter').only().click();
$('#counter').only().hasText('Count: 4'); // Sameness check 2

// Disable disabled
window.State.disabled(false);
$('#counter').only().checkProps({ id: 'counter' });
$('#counter').only().hasText('Count: 4'); // Sameness check 3

// Click - should increment
$('#counter').only().click();
$('#counter').only().hasText('Count: 5');

// Final verification
$('#counter').only().checkProps({ id: 'counter' });
$('#counter').only().hasText('Count: 5');
