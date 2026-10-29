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

let guarded = false

function onBeforeUnload(event) {
    event.preventDefault()
    event.returnValue = ""
}

function pushGuard() {
    history.pushState({ secretWordGuard: true }, "")
}

function unguard() {
    guarded = false
    window.removeEventListener("beforeunload", onBeforeUnload)
}

app.ports?.protectGame?.subscribe((protect) => {
    if (protect) {
        if (guarded) return
        guarded = true
        window.addEventListener("beforeunload", onBeforeUnload)
        pushGuard()
    } else {
        if (!guarded) return
        unguard()
        if (history.state?.secretWordGuard) history.back()
    }
})

app.ports?.leaveGame?.subscribe(() => {
    if (!guarded) return
    unguard()
    // -1 tira a guarda, -2 é o voltar que o jogador pediu
    history.go(-2)
})

window.addEventListener("popstate", () => {
    if (!guarded) return
    pushGuard()
    app.ports?.backPressed?.send(null)
})
