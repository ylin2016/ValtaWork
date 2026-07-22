/**
 * CleaningReminder — configuration
 *
 * Secrets (Twilio SID/token/number) do NOT go here — they live in Script
 * Properties (Project Settings → Script properties). See README.md.
 */

const CONFIG = {
  // How many days ahead to scan. 1 = tomorrow (the next-day schedule).
  DAYS_AHEAD: 1,

  // Date used by previewDate() when you Run it from the editor (the Run button
  // can't pass an argument). Set this, pick previewDate, and Run. Format YYYY-MM-DD.
  PREVIEW_DATE: '2026-07-16',

  // SAFETY SWITCH.
  //   true  → parse events and LOG the messages that WOULD be sent (no texts).
  //   false → actually send SMS via Twilio.
  // Keep this true until the logs look right, then flip to false.
  DRY_RUN: true,

  // Leader / dispatcher number(s) that should see EVERY cleaner's schedule. Each
  // gets a 1:1 copy of every cleaner's message (same as the cleaners themselves).
  // E.164, e.g. ['+12065550100'].
  LEADER_PHONES: [],

  // Which week runWeekly() summarizes. Weeks run MONDAY–SUNDAY.
  //   'upcoming' → today's week if it's Monday, else next week. Best for the
  //                Monday trigger: Monday covers the week starting that day, and a
  //                manual mid-week run looks ahead to next week.
  //   'this'     → always the Mon–Sun week that contains today.
  //   'next'     → always the week after this one.
  // To just LOOK without changing this, run previewThisWeek() / previewNextWeek().
  WEEKLY_TARGET: 'upcoming',

  // A calendar event is treated as a cleaning JOB only if it has a real title.
  // The empty "(No title)" 11am–4pm / 4–10pm shift blocks are ignored.
  SKIP_UNTITLED: true,

  // BACK-TO-BACK is a property of the EVENT (the shift), not the unit. The purple
  // morning shift (11am–4pm) is the back-to-back turn — a new guest checks in that
  // day, so it must finish before arrival. The yellow evening shift (4pm–10pm) is
  // not. We classify by the event's START hour (24h, calendar timezone): an event
  // that starts before this hour is back-to-back. 15 (3pm) cleanly splits the
  // 11am shift from the 4pm shift.
  BACKTOBACK_BEFORE_HOUR: 15,

  // For non-back-to-back (yellow, evening) shifts there is no same-day check-in,
  // so the deadline is the next day's check-in. Show this as the end of the window
  // instead of the event's literal end time (e.g. "4:00 PM–11:00 AM (next day)").
  NONB2B_END_LABEL: '11:00 AM (next day)',

  // Business name shown at the top of each text message.
  BRAND: 'Valta Realty Cleaning Schedule',

  // ----------------------------------------------------------------------------
  // Route planning (Maria only — previewMariaRoutes / runMariaRoutes).
  // Geocodes each stop's address, sizes the day's workload to decide how many cars
  // are needed (up to MAX_CARS), staffs each car with 2–4 people (the fewest that
  // finish within MAX_HOURS_PER_PERSON — more people clean proportionally faster),
  // and orders each crew's stops by shortest travel from the base and back.
  // ----------------------------------------------------------------------------
  ROUTING: {
    // Depot: crews start AND end here each day (round trip). REQUIRED for routing —
    // fill in your office/home base. Leave '' to skip base legs (planner still runs
    // but can't tell you the first/last drive). Full address, e.g. "123 Main St, Bellevue, WA".
    BASE_ADDRESS: '12834 24th Ave S, SeaTac, WA 98168',

    // Crew size is chosen per car: the planner sizes the day's workload to decide how
    // many cars are needed (up to MAX_CARS), then staffs each with the FEWEST people
    // in [MIN_CREW_PER_CAR, MAX_CREW_PER_CAR] that finish its route within the day.
    MIN_CREW_PER_CAR: 2,        // smallest crew in a car.
    MAX_CREW_PER_CAR: 4,        // largest crew in a car.
    MAX_HOURS_PER_PERSON: 8,    // workday cap (clock-hours). A crew works together, so this is the car's clock-day.
    MAX_UNITS_PER_CAR: 6,       // hard cap on units (stops) per car per day.
    MAX_CARS: 3,                // most cars available. Work beyond this is flagged as unassigned.

    // Units at the SAME ADDRESS always ride in one car (kept together, never split
    // across crews) — even if that address alone exceeds MAX_UNITS_PER_CAR or the
    // 8-hour day, in which case the car overrides those caps. This keeps a whole
    // building (e.g. all of Elektra at 1400 Hubbell Pl) as one crew's job. No knob —
    // it's driven by the address on each listing.

    // Cleaning times below are clock-minutes for a CALIBRATION_CREW-person crew. The
    // planner converts them to labor (time × CALIBRATION_CREW person-min) and divides
    // by the car's actual crew size, so a 1BR/1BA = 60 min at 2 people, 40 at 3, 30 at 4.
    CALIBRATION_CREW: 2,

    // Turnover cleaning time = base + per-bedroom + per-bathroom. Anchored so a
    // 1BR/1BA = 60 min for 2 people (25 + 15 + 20); each extra bed adds 15, each
    // extra bath adds 20.
    CLEAN_BASE_MIN: 25,
    CLEAN_PER_BEDROOM_MIN: 15,
    CLEAN_PER_BATHROOM_MIN: 20,

    // Residential & move-in/out have only an address (no bed/bath) — fixed minutes
    // each (2-person clock-time; scaled by crew size like turnovers).
    RESIDENTIAL_MIN: 120,       // 2h @ 2 people.
    MOVEINOUT_MIN: 120,         // 2h @ 2 people.

    // Travel model: straight-line miles between geocoded stops ÷ this speed, then
    // inflated by ROAD_FACTOR to approximate real driving. Keeps API calls to one
    // geocode per unique address (cached in Script Properties).
    AVG_SPEED_MPH: 28,
    ROAD_FACTOR: 1.3,           // road distance ≈ 1.3 × straight-line.

    // Optional: a Google Sheet (columns Listing, Address, Bedrooms, Bathrooms) that
    // overrides/extends the embedded Listings.gs table. '' = use embedded table only.
    LISTINGS_SHEET_ID: '',
  },
};
