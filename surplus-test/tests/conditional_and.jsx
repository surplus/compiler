import S from '@surplus/s';

const Root = () => {
	window.State = {
		show: S.data(false)
	};

	return (
		<div>
			{State.show() && <span id="visible">I am visible</span>}
			<div id="always">Always here</div>
		</div>
	);
};

export default <Root />;
