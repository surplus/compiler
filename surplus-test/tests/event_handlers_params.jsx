import S from '@surplus/s';

const Root = () => {
	window.State = {
		targetId: S.data(''),
		currentTargetId: S.data(''),
		defaultPrevented: S.data(false),
		key: S.data('')
	};

	return (
		<div>
			<form id="test-form" on:submit={(e) => {
				e.preventDefault();
				window.State.defaultPrevented(e.defaultPrevented);
			}}>
				<button type="submit" id="submit-btn">Submit</button>
			</form>
			<button
				id="target-btn"
				on:click={(e) => {
					window.State.targetId(e.target.id);
					window.State.currentTargetId(e.currentTarget.id);
				}}
			>
				Click me
			</button>
			<input
				id="key-input"
				on:keydown={(e) => window.State.key(e.key)}
			/>
		</div>
	);
};

export default <Root />;
