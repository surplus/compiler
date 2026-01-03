import S from '@surplus/s';

const Root = () => {
	window.State = {
		disabled: S.data(true),
		checked: S.data(false),
		readonly: S.data(true)
	};

	return (
		<div>
			<input type="text" disabled={State.disabled() || undefined} id="text-input" />
			<input type="checkbox" checked={State.checked() || undefined} id="checkbox" />
			<input type="text" readonly={State.readonly() || undefined} id="readonly-input" />
			<button disabled={State.disabled() || undefined} id="btn">Click me</button>
		</div>
	);
};

export default <Root />;
