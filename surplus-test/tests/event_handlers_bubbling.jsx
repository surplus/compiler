import S from '@surplus/s';

const Root = () => {
	window.State = {
		parentClicks: S.data(0),
		childClicks: S.data(0),
		stopProp: S.data(false)
	};

	return (
		<div
			id="parent"
			on:click={() => window.State.parentClicks(window.State.parentClicks() + 1)}
		>
			<button
				id="child"
				on:click={(e) => {
					window.State.childClicks(window.State.childClicks() + 1);
					if (window.State.stopProp()) {
						e.stopPropagation();
					}
				}}
			>
				Click child
			</button>
		</div>
	);
};

export default <Root />;
