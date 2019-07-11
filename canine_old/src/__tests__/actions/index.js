import configureMockStore from 'redux-mock-store'
import promise from 'redux-promise'
import * as actions from 'actions'
import fetchMock from 'fetch-mock'

const middlewares = [promise]
const mockStore = configureMockStore(middlewares)

describe('ActionCreatorのテスト', () => {
  afterEach(() => {
    fetchMock.reset()
    fetchMock.restore()
  })

  it('fetchCompilerList で、意図したアクションが dispatch されていることを確認する', () => {
    const body = []
    fetchMock.getOnce('https://wandbox.org/api/list.json', {
      body: body,
      headers: { 'content-type': 'application/json' }
    })

    const expectedActions = [
      { type: actions.FETCH_COMPILER_LIST_INIT },
      { type: actions.FETCH_COMPILER_LIST, payload: body }
    ]
    const store = mockStore({})

    return store
      .dispatch(actions.fetchCompilerList(store.dispatch))
      .then(() => {
        expect(store.getActions()).toEqual(expectedActions)
      })
  })
})
