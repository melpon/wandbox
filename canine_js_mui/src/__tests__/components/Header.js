import React from 'react'
import Header from 'components/Header'
import renderer from 'react-test-renderer'

test('ヘッダーのスナップショットが意図せず変わっていない', () => {
  const component = renderer.create(<Header />)
  const tree = component.toJSON()
  expect(tree).toMatchSnapshot()
})
