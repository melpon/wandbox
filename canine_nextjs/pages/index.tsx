import React from 'react'
import { AppContexts } from '~/apps/AppContexts'
import { Wandbox } from '~/apps/Wandbox'

export default function Home() {
  return (
    <AppContexts>
      <Wandbox />
    </AppContexts>
  )
}
