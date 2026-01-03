import S from '@surplus/s';

const Root = () => {
	window.State = {
		id: S.data('elem-1'),
		className: S.data('box'),
		dataValue: S.data('42'),
		title: S.data('Hover me')
	};

	return (
		<div
			id={State.id()}
			class={State.className()}
			data-value={State.dataValue()}
			title={State.title()}
		>
			Content
		</div>
	);
};

export default <Root />;
