import S from '@surplus/s';

const Root = () => {
	window.State = {
		clicks: S.data(0),
		enters: S.data(0),
		leaves: S.data(0),
		inputs: S.data(0),
		value: S.data('')
	};

	return (
		<div>
			<button
				id="click-btn"
				on:click={() => State.clicks(State.clicks() + 1)}
			>
				Clicks: {State.clicks()}
			</button>
			<div
				id="hover-div"
				on:mouseenter={() => State.enters(State.enters() + 1)}
				on:mouseleave={() => State.leaves(State.leaves() + 1)}
			>
				Hover me
			</div>
			<input
				id="text-input"
				on:input={(e) => {
					State.inputs(State.inputs() + 1);
					State.value(e.target.value);
				}}
			/>
		</div>
	);
};

export default <Root />;
