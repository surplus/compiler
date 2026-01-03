import S from '@surplus/s';

const Root = () => {
	window.State = {
		mode: S.data('a')
	};

	return (
		<div>
			{State.mode() === 'a' ? <span id="mode-a">Mode A</span> : <span id="mode-b">Mode B</span>}
		</div>
	);
};

export default <Root />;
