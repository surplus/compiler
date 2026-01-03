import S from '@surplus/s';

const Root = () => {
	window.State = {
		circleClicks: S.data(0),
		rectClicks: S.data(0)
	};

	return (
		<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100" viewBox="0 0 100 100">
			<circle
				id="test-circle"
				cx="25"
				cy="50"
				r="20"
				fill="red"
				on:click={() => window.State.circleClicks(window.State.circleClicks() + 1)}
			/>
			<rect
				id="test-rect"
				x="60"
				y="30"
				width="30"
				height="40"
				fill="blue"
				on:click={() => window.State.rectClicks(window.State.rectClicks() + 1)}
			/>
		</svg>
	);
};

export default <Root />;
