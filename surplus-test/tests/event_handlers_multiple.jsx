import S from '@surplus/s';

const Root = () => {
	window.State = {
		handler1: S.data(0),
		handler2: S.data(0),
		handler3: S.data(0)
	};

	return (
		<button
			id="multi-handler"
			on:click={() => State.handler1(State.handler1() + 1)}
			on:click={() => State.handler2(State.handler2() + 1)}
			on:click={() => State.handler3(State.handler3() + 10)}
		>
			Click me
		</button>
	);
};

export default <Root />;
