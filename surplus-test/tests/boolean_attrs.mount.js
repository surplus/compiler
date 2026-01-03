// Initial state: disabled=true, checked=false, readonly=true
$('#text-input').only().exactProps({ type: 'text', disabled: true, id: 'text-input' });
$('#checkbox').only().exactProps({ type: 'checkbox', id: 'checkbox' });
$('#readonly-input').only().exactProps({ type: 'text', readonly: true, id: 'readonly-input' });
$('#btn').only().exactProps({ disabled: true, id: 'btn' });

// Toggle disabled to false
window.State.disabled(false);
$('#text-input').only().exactProps({ type: 'text', id: 'text-input' });
$('#btn').only().exactProps({ id: 'btn' });

// Toggle checked to true
window.State.checked(true);
$('#checkbox').only().exactProps({ type: 'checkbox', checked: true, id: 'checkbox' });

// Toggle readonly to false
window.State.readonly(false);
$('#readonly-input').only().exactProps({ type: 'text', id: 'readonly-input' });

// Toggle everything back
window.State.disabled(true);
window.State.checked(false);
window.State.readonly(true);
$('#text-input').only().exactProps({ type: 'text', disabled: true, id: 'text-input' });
$('#checkbox').only().exactProps({ type: 'checkbox', id: 'checkbox' });
$('#readonly-input').only().exactProps({ type: 'text', readonly: true, id: 'readonly-input' });
$('#btn').only().exactProps({ disabled: true, id: 'btn' });
