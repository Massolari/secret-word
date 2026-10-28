import { Elm } from './Main.elm'

const app = Elm.Main.init({
    node: document.getElementById("elm"),
})

app.ports?.vibrate?.subscribe((pattern) => {
    try {
        navigator.vibrate?.(pattern)
    } catch (_) {}
})

let wantsWakeLock = false
let wakeLock = null
let pendingRequest = null

async function requestWakeLock() {
    if (!navigator.wakeLock || pendingRequest || (wakeLock && !wakeLock.released)) return
    try {
        pendingRequest = navigator.wakeLock.request("screen")
        const lock = await pendingRequest
        if (!lock.released) {
            lock.addEventListener("release", () => {
                if (wakeLock === lock) wakeLock = null
            })
            wakeLock = lock
        }
    } catch (_) {
    } finally {
        pendingRequest = null
    }
    if (!wantsWakeLock) releaseWakeLock()
}

function releaseWakeLock() {
    wakeLock?.release().catch(() => {})
    wakeLock = null
}

app.ports?.keepAwake?.subscribe((keep) => {
    wantsWakeLock = keep
    keep ? requestWakeLock() : releaseWakeLock()
})

document.addEventListener("visibilitychange", () => {
    if (wantsWakeLock && document.visibilityState === "visible") requestWakeLock()
})
