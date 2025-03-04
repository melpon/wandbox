
async function openDB(): Promise<IDBDatabase> {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open("cache", 3);

    request.onupgradeneeded = () => {
      const db = request.result;
      if (db.objectStoreNames.contains("cache")) {
        db.deleteObjectStore("cache");
      }
      db.createObjectStore("cache");
    };

    request.onsuccess = () => {
      resolve(request.result);
    };

    request.onerror = () => {
      reject(request.error);
    };
  });
}

// IndexedDB にデータを保存する
export async function saveToIndexedDB(key: string, data: Blob): Promise<void> {
  const db = await openDB();
  const transaction = db.transaction("cache", "readwrite");
  const store = transaction.objectStore("cache");
  store.put(data, key);
  return new Promise<void>((resolve, reject) => {
    transaction.oncomplete = () => resolve();
    transaction.onerror = () => reject(transaction.error);
  });
}

// IndexedDB からデータを取得する
export async function getFromIndexedDB(key: string): Promise<Blob | undefined> {
  const db = await openDB();
  const transaction = db.transaction("cache", "readonly");
  const store = transaction.objectStore("cache");
  const req = store.get(key) as IDBRequest<Blob | undefined>;
  return new Promise<Blob | undefined>((resolve, reject) => {
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}
