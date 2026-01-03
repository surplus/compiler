import S from '@surplus/s';

const Root = () => {
	window.State = { show: S.data(null) };

	let child = S(() => {
		switch (State.show()) {
			case 'A':
				return <span>A</span>;
			case 'B':
				return <span>B</span>;
			default:
				return <span>Default</span>;
		}
	});

	return <div>Current child: {child()}</div>;
};

export default <Root />
