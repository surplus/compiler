const Greeting = ({ id, name, age }) => (
	<div id={id}>Hello {name}, you are {age} years old</div>
);

const props = {
	id: 'test-elem',
	name: 'Alice',
	age: 30
};

export default <Greeting {...props} />;
