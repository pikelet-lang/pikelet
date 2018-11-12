const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');

const Container = CompLibrary.Container;

const siteConfig = require(`${process.cwd()}/siteConfig.js`);

class Try extends React.Component {
  render() {
    return (
      <div className="mainContainer">
        <Container padding={['bottom', 'top']}>
          <div className="prose">
            <h1>Try Pikelet</h1>
            <p>Coming soon...</p>
          </div>
        </Container>
      </div>
    );
  }
}

module.exports = Try;
