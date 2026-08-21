# Importing Yacinde Bookings into Guesty via iCal

You have 8 iCal feeds (one per unit) covering all 140 reservations. Guesty
subscribes to each feed by URL and blocks those dates on the listing's calendar,
showing the guest name and source on each booking.

There are two steps: **(1) host the feeds** so Guesty can reach them, then
**(2) subscribe each Guesty listing** to its feed.

---

## Step 1 — Host the feeds (Netlify Drop, ~2 minutes)

1. Open **https://app.netlify.com/drop** in your browser.
2. Drag the entire **`guesty_ical_deploy`** folder onto the page.
3. Netlify uploads it and gives you a site address like
   `https://calm-otter-12345.netlify.app`.
4. (Recommended) Click **"Sign up to keep this site"** so it doesn't expire.
   A free account keeps the URLs permanent.

Your 8 feed URLs will then be (replace the site name with yours):

```
https://YOUR-SITE.netlify.app/Yacinde_B1.ics
https://YOUR-SITE.netlify.app/Yacinde_B2.ics
https://YOUR-SITE.netlify.app/Yacinde_B3.ics
https://YOUR-SITE.netlify.app/Yacinde_B4.ics
https://YOUR-SITE.netlify.app/Yacinde_B6.ics
https://YOUR-SITE.netlify.app/Yacinde_E1.ics
https://YOUR-SITE.netlify.app/Yacinde_F1.ics
https://YOUR-SITE.netlify.app/Yacinde_F5.ics
```

Tip: open `https://YOUR-SITE.netlify.app` to see a page listing all 8 links.

---

## Step 2 — Subscribe each Guesty listing to its feed

For **each** of the 8 units, in Guesty:

1. Go to **Listings** and open the listing (e.g. Yacinde B1).
2. Open the **Calendar** tab.
3. Find **Import calendar** / **Connect a calendar** (also called
   "Link a calendar" or "iCal import", depending on your Guesty version).
4. Paste that unit's `.ics` URL from the list above.
5. Save. Guesty fetches the feed and blocks the dates.

Repeat for all 8 units, matching each listing to the correctly named `.ics` URL.

Guesty re-checks external iCal feeds periodically (typically every few hours),
so blocks appear shortly after you connect each one.

---

## Important notes

- **This is a one-time backfill of existing bookings.** The feeds are a static
  snapshot. If new reservations come in later, this feed will *not* update on
  its own — for ongoing sync, connect your channels (Airbnb/VRBO) or Guesty's
  own booking flow. Ask me to regenerate the feeds if you get an updated export.
- **iCal creates calendar *blocks*, not full reservations.** Guest names and
  source show on the block, but prices and guest contact details are not stored
  as reservation records. If you need those as true reservations, the Guesty
  Open API route is the alternative — just ask.
- **Keep the Netlify site live** while the feeds are connected. If you delete it,
  Guesty can no longer reach the feeds.
- A cleaned spreadsheet of all 140 bookings (`Yacinde_Reservations_Clean.xlsx`)
  is in your folder for reference or the API route.
