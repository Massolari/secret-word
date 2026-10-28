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

async function requestWakeLock() {
    if (!navigator.wakeLock || wakeLock) return
    try {
        wakeLock = await navigator.wakeLock.request("screen")
        wakeLock.addEventListener("release", () => {
            wakeLock = null
        })
        if (!wantsWakeLock) releaseWakeLock()
    } catch (_) {
        wakeLock = null
    }
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
