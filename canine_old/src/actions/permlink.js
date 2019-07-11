// @flow
const makeType = name => `canine/permlink/${name}`

export const FETCH_PERMLINK_INIT = makeType('FETCH_PERMLINK_INIT')
export const FETCH_PERMLINK = makeType('FETCH_PERMLINK')
export async function fetchPermlink(dispatch: Function, permlink: string) {
  dispatch({
    type: FETCH_PERMLINK_INIT
  })

  const payload = await fetch(`https://wandbox.org/api/permlink/${permlink}`)
  return {
    type: FETCH_PERMLINK,
    payload: await payload.json()
  }
}
