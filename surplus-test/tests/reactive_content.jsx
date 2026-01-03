import S from '@surplus/s';

const Root = () => {
	window.State = {
		name: S.data('World'),
		count: S.data(42)
	};

	return (
		<div>
			Hello <strong>{State.name()}</strong>, you have {State.count()} messages.
			<br />
			Your score: {State.count() * 2}
			<span> (doubled)</span>
		</div>
	);
};

export default <Root />;
