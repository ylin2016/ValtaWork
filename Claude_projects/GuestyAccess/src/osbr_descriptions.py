"""Write one standardized Word listing description per OSBR cottage (1-12).

Shared sections are defined once below; only Summary, The Space and a few
per-cottage rule lines vary. Output: GuestyAccess/Output/osbr_listing_descriptions/
"""
from pathlib import Path

import docx
from docx.shared import Pt

OUT = Path("/Users/ylin/ValtaWork/Claude_projects/GuestyAccess/Output/osbr_listing_descriptions")

# ------------------------------------------------------------ shared blocks

SUMMARY_SMALL = ("Whether you're here for a quiet solo retreat or a relaxing trip for two, this simple "
                 "beachside getaway offers what you need for a peaceful stay by the sea.")
SUMMARY_FAMILY = ("Whether you're here for a relaxing getaway with friends or family, this simple "
                  "beachside getaway offers what you need for a peaceful stay by the sea.")
SUMMARY_CLOSE = "Take in the fresh ocean breeze and enjoy some downtime in this convenient, no-fuss spot."

OUTDOOR = ("The cottage has a small patio for a bit of outdoor relaxation. The resort offers a shared "
           "playground and a few recreational activities like horseshoes and fire pits for an evening "
           "bonfire. Walk just five minutes to the beach, where you can enjoy beachcombing, casual "
           "strolls, or just taking in the sea breeze.")
OUTDOOR_BULLETS_TAIL = ["Shared playground and fire pit", "5-minute walk to the beach"]

NEIGHBORHOOD = [
    "Located just a short 5-minute walk from Grayland Beach, {unit} offers easy access to serene sandy "
    "shores perfect for a variety of activities. Whether you're sunbathing, building sandcastles, clam "
    "digging, beachcombing, enjoying beachside picnics, or having cozy bonfires, Grayland Beach ensures "
    "endless fun and relaxation for everyone.",
    "For your convenience, the nearest grocery store, the Local Store, is just 3 miles away, while "
    "Shop ‘n Kart is just 5.8 miles up north, providing all the essentials you might need during your "
    "stay. For dining options, the area boasts several delightful restaurants. The Local Bar & Grill, a "
    "local favorite, is just 1.1 miles away from the property. Bennett's Fish Shack, another favorite, "
    "is 7.5 miles away and offers fresh seafood in a casual setting.",
    "In addition to the beach and dining, there are several interesting places to explore. A nearby "
    "local attraction, The Cranberry Museum, is just 1.6 miles from the property. For those interested "
    "in history, the Westport Maritime Museum, just 7 miles away, provides fascinating insights into "
    "the local maritime heritage. The Westport Winery Garden Resort, located 12.4 miles away, offers "
    "wine tasting, beautiful gardens, a delightful restaurant, and the famous Mermaid Museum.",
    "With its convenient location and a variety of nearby amenities and attractions, {unit} at Ocean "
    "Spray Beach Resort offers the perfect base for a relaxing and enjoyable stay.",
]

ACCESS = "Guests have full access to the property and shared access to resort amenities."

TRANSIT = ("Having a car is essential to fully enjoy the local attractions, and ample parking is available "
           "onsite. The main highway, WA-105, offers easy access to nearby towns and points of interest. "
           "While public transportation is limited, Grays Harbor Transit provides regional bus services, "
           "with the nearest stop located in Westport, about a 20-minute drive from the property.")

NOTE_AC = ("Please note, there is no air conditioning. However, our location just a 5-minute walk from the "
           "Pacific Ocean ensures a constant, refreshing sea breeze to keep you cool.")
NOTE_PETS = "Up to 2 pets are allowed with a $50 per pet fee or $100 for 2 pets."
NOTE_OLDER = "This is an older cottage by the ocean. Expect rustic seaside charm with a touch of wildlife and sand."
NOTE_KITCHEN = ("Kitchen is small, but comes equipped with a small drip coffee maker, microwave, "
                "refrigerator/freezer, and a stove/oven. Pots, pans and bakeware are limited due to space, so "
                "if you're planning to cook larger meals, please bring your own cookware and bakeware. We do "
                "provide plenty of plates, bowls and silverware along with knives, can opener and various "
                "other kitchenware items.")
NOTE_COMMON = "The common areas, including the media room, gym, and ping pong area, are open from 8:00 AM to 10:00 PM."

RULES_TOP = [
    "Please refrain from hosting parties or events during your stay.",
    "Only individuals registered on the reservation are permitted on the premises. An additional fee of "
    "$20 per unregistered guest per day will apply.",
    "Smoking, vaping, or burning of any kind, including candles or incense, is strictly prohibited "
    "indoors and on the patio or porch.",
    "We highly value the peace and tranquility of our neighborhood. Kindly maintain minimal noise levels, "
    "particularly during our designated quiet hours from 10 PM to 7 AM.",
]
FINE = "* Any violations of the above will result in a $500 fine, and guests will be asked to leave without any refund."
RULE_PETS = "We warmly welcome a maximum of 2 pets per stay, accompanied by a $50 per pet fee."
RULE_PRIVATE = "No access to any areas marked as private or to other cottages at the resort."
RULES_BOTTOM = [
    "Please exercise care and consideration during your stay to avoid excess cleaning or damages. "
    "Charges may be applied for any necessary repair or replacement.",
    "Should you inadvertently leave behind personal belongings requiring shipping, we offer this service "
    "for a nominal fee of $50 plus the actual shipping cost. Shipping + handling fee must be prepaid "
    "before shipment.",
]
CHECKOUT = [
    "Return all kitchen items to their original location or follow the existing labeling.",
    "Run the dishwasher if needed.",
    "Collect all trash and place it in the outside bin.",
    "Place all used towels in the washer and start a load.",
    "Close all windows and lock all exterior doors.",
]
RULES_END = [
    "No access to the mailbox. Please rent a PO box. There is a $50 charge if you want us to go there "
    "and get the mail for you.",
    "If a guest's actions result in a fine from any utility company, government agency, HOA, parking "
    "enforcement or other party, the guest will be responsible for the full amount of the fine plus a "
    "$100 administrative fee.",
]

# ------------------------------------------------------------ per cottage
# kind: "small" (studio / 1BR) or "family" (2BR) picks the summary sentence.

C = {
    1: dict(kind="small",
            opener="Enjoy a comfortable and laid-back stay in this cozy 1-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["312 sq ft", "1 Bedroom, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "Cottage 1 at Ocean Spray Beach Resort is a basic and comfortable spot for a quick beach getaway. The small living area has a loveseat, an armchair and a flat-screen TV for unwinding after a day at the beach. The full kitchen has what you need for easy meal prep.",
                    ["Loveseat and an armchair", "Flat-screen TV", "Fully equipped kitchen"]),
                   ("Bedroom and Bathroom",
                    "The bedroom features a comfortable queen-sized bed, ideal for couples or solo travelers looking to unwind. The bathroom has all the essentials you’ll need for your stay.",
                    ["Queen bed", "1 Bathroom"])],
            outdoor="Private patio"),
    2: dict(kind="small",
            opener="Enjoy a comfortable and laid-back stay in this cozy 1-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["435 sq ft", "1 Bedroom, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "This delightful haven is perfect for cozy living, featuring a comfortable 2-seater couch ideal for couples to unwind together, along with a flat-screen TV for entertainment. Enjoy cooking in the fully equipped kitchen and gather at the cozy breakfast nook for conversation and memories.",
                    ["Comfortable 2-seater couch", "Flat-screen TV", "Fully equipped kitchen"]),
                   ("Bedroom and Bathroom",
                    "Enjoy a cozy bedroom with a queen bed and a well-equipped bathroom. It also has a closet stocked with linens, pillows, and blankets. Perfect for unwinding after a day at the beach, this delightful retreat ensures a comfortable stay.",
                    ["1 Bedroom with queen bed", "1 Bathroom with shower"])],
            outdoor="Private porch"),
    3: dict(kind="small",
            opener="Enjoy a comfortable and laid-back stay in this cozy studio cottage just a short 5-minute walk from the beach.",
            facts=["360 sq ft", "Studio layout with 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "In the living room, you'll find a cozy sofa and an armchair, perfect for relaxing and enjoying the TV. The bed is conveniently located nearby, making it easy to access everything. The fully equipped kitchen meets all your needs for a hassle-free stay.",
                    ["Fully equipped kitchen", "Cozy sofa and armchair", "Flat-screen TV"]),
                   ("Bedroom and Bathroom",
                    "The queen bed in the studio is thoughtfully positioned to maximize the cottage layout, ensuring a comfortable experience. The bathroom is equipped with essential amenities, offering a serene retreat for your relaxation needs.",
                    ["1 Queen bed", "1 Bathroom with shower"])],
            outdoor="Private patio"),
    4: dict(kind="family",
            opener="Enjoy a comfortable and laid-back stay in this 2-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["700 sq ft", "2 Bedrooms, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "The living room features two brown 2-seater sofas and a single sofa, creating a cozy area for relaxing and socializing. A flat-screen TV adds to the entertainment options. The adjacent kitchen is fully equipped, making it easy to prepare meals. A round dining table comfortably seats four, perfect for sharing meals and conversations.",
                    ["Multiple seating arrangements", "Flat-screen TV", "Fully equipped kitchen", "Dining for 4"]),
                   ("Bedrooms and Bathroom",
                    "The first bedroom offers a serene retreat with a comfortable queen bed, ensuring a restful night's sleep. The second bedroom is ideal for additional guests or family members, featuring two full beds. The cottage includes a well-appointed bathroom with all necessary amenities to start your day refreshed.",
                    ["Queen bed in bedroom 1", "2 full beds in bedroom 2", "1 Bathroom with shower"])],
            outdoor="Private patio"),
    5: dict(kind="family",
            opener="Enjoy a comfortable and laid-back stay in this 2-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["700 sq ft", "2 Bedrooms, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "The living room features cozy sofas, a loveseat, and a recliner, all centered around a TV for entertainment. The dining area comfortably seats up to six, creating the perfect setting for family meals. The fully equipped kitchen makes meal preparation a joyful experience.",
                    ["Multiple seating arrangements", "Flat-screen TV", "Fully equipped kitchen", "Dining for 6"]),
                   ("Bedrooms and Bathroom",
                    "Cottage 5 is a peaceful escape that invites you to unwind in its two thoughtfully arranged bedrooms. The first bedroom features a queen bed and a simple dressing table, creating a serene space for morning routines. The second bedroom is ideal for additional guests or family members, featuring two full beds. The bathroom is equipped with essential amenities, offering a serene retreat for your relaxation needs.",
                    ["Queen bed in bedroom 1", "2 full beds in bedroom 2", "1 Bathroom with shower"])],
            outdoor="Private patio"),
    6: dict(kind="family",
            opener="Enjoy a comfortable and laid-back stay in this 2-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["600 sq ft", "2 Bedrooms, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "Step into the living room, where a cozy sofa and an inviting armchair await, along with a TV for movie nights and entertainment. The dining area comfortably seats four, making it an ideal spot for sharing meals. The fully equipped kitchen invites you to cook up delicious meals, turning preparation into a joyful experience.",
                    ["Fully equipped kitchen", "Multiple seating arrangements", "Flat-screen TV", "Dining for 4"]),
                   ("Bedrooms and Bathroom",
                    "Cottage 6 provides a tranquil escape with two elegantly furnished bedrooms, each adorned with a queen-size bed and two windows that invite refreshing natural sunlight during the day. In the evening, bask in a cozy ambiance illuminated by bedroom lamps. The bathroom enhances your stay with modern comfort and convenience.",
                    ["2 Bedrooms with queen beds", "1 Bathroom with shower"])],
            outdoor="Private patio"),
    7: dict(kind="family",
            opener="Enjoy a comfortable and laid-back stay in this 2-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["600 sq ft", "2 Bedrooms, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "The living area is cozy and inviting, with a comfortable sofa, two armchairs, and a flat-screen TV for your entertainment. The cottage features a fully equipped kitchen, making meal prep easy. Enjoy intimate meals at the breakfast table, perfect for two, where you can relax and connect. This welcoming space is ideal for creating lasting memories together.",
                    ["Fully equipped kitchen", "Living room with multiple seating arrangements", "Flat-screen TV", "Dining for 2"]),
                   ("Bedrooms and Bathroom",
                    "Cottage 7 offers a serene escape with two beautifully arranged bedrooms. The first bedroom features a queen bed and a stylish dressing table, creating a perfect space to unwind in comfort. The second bedroom includes two full-sized beds, making it ideal for family or friends. The cottage also has a bathroom equipped with all the essentials, ensuring a convenient and relaxing stay.",
                    ["Queen bed in bedroom 1", "2 full beds in bedroom 2", "1 Bathroom with shower"])],
            outdoor="Private patio"),
    8: dict(kind="family",
            opener="Enjoy a comfortable and laid-back stay in this 2-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["600 sq ft", "2 Bedrooms, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "The fully equipped kitchen makes meal preparation a pleasure, while the inviting living area features a comfortable loveseat and an armchair, ideal for cozy evenings spent watching TV. Share meals at the delightful breakfast table for two, nestled by the window, where you can savor the views and create lasting memories together.",
                    ["Fully equipped kitchen", "Living room with multiple seating arrangements", "Flat-screen TV", "Dining for 2"]),
                   ("Bedrooms and Bathroom",
                    "Embrace a classic Americana retreat in this charming beachside vacation cottage. The first bedroom features a queen bed and a vanity mirror cabinet, offering both comfort and style. The second bedroom, perfect for families or friends, includes two full-sized beds. The cottage also has a bathroom stocked with essentials, a convenient closet, and is furnished with linens, pillows, blankets, and sheets in a classic Americana theme.",
                    ["Queen bed in bedroom 1", "2 full beds in bedroom 2", "1 Bathroom with shower"])],
            outdoor="Private patio"),
    9: dict(kind="small",
            opener="Enjoy a comfortable and laid-back stay in this cozy 1-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
            facts=["500 sq ft", "1 Bedroom, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
            space=[("Living, Dining, and Kitchen",
                    "Cottage 9 at Ocean Spray Beach Resort is a basic and comfortable spot for a quick beach getaway. The small living area has a loveseat, an armchair and a flat-screen TV for unwinding after a day at the beach. The full kitchen has what you need for easy meal prep.",
                    ["Fully equipped kitchen", "Loveseat and an armchair", "Flat-screen TV", "Dining area"]),
                   ("Bedroom and Bathroom",
                    "The bedroom features a comfortable queen-sized bed, ideal for couples or solo travelers looking to unwind. The bathroom has all the essentials you’ll need for your stay.",
                    ["Queen bedroom with closet", "1 Bathroom with shower"])],
            outdoor="Private porch"),
    10: dict(kind="small",
             opener="Enjoy a comfortable and laid-back stay in this cozy studio cottage just a short 5-minute walk from the beach.",
             facts=["500 sq ft", "Studio layout with 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
             space=[("Studio",
                     "This cozy studio unit includes a well-equipped kitchen for all your cooking needs. It features a queen-sized bed with a TV mounted on the wall and a leather sofa bed for extra seating. There's a small dining area near the kitchen that seats four, perfect for meals or gatherings. The unit also has a bathroom for your convenience. Enjoy a relaxing stay in this inviting space!",
                     ["1 Queen bed", "Leather sofa bed", "Fully equipped kitchen", "Dining for 4", "1 Bathroom with shower"])],
             outdoor="Private porch"),
    11: dict(kind="family", unit="the tiny house",
             opener="Welcome to our cozy loft tiny house at Ocean Spray Beach Resort in Grayland, just a short 5-minute walk from the beach.",
             facts=["450 sq ft", "1 Bedroom, 1 Bathroom", "1 lofted sleeping space and 1 lofted seating area", "Fully equipped kitchen", "5-minute walk to the beach"],
             space=[("Living, Dining, and Kitchen",
                     "Cottage 11 at Ocean Spray Beach Resort is a cozy spot for a quick beach getaway. It has a small loveseat in the kitchen area and a flat-screen TV facing the dining area, which seats four. The full kitchen is perfect for easy meal prep, making it a great place to unwind after a day at the beach!",
                     ["Fully equipped kitchen", "Dining for 4", "Flat-screen TV", "Loveseat"]),
                    ("Bedroom and Bathroom",
                     "Retreat to the peaceful bedroom with a plush queen-sized bed and soft linens. For extra sleeping space, there's a cozy loft area accessible by a ladder. Adjacent to the kitchen, climb the ladder to a lofted seating area with a comfortable tatami chair, perfect for reading a book or quiet time. The bathroom features a shower with a tub, and essential toiletries for a refreshing start to your day.",
                     ["1 Queen bed", "1st lofted area with additional sleeping space", "2nd lofted seating area with a comfortable tatami chair", "1 Bathroom with tub and shower"])],
             outdoor="Private porch",
             first_note="The lofts are reached by a rung ladder only."),
    12: dict(kind="family",
             opener="Enjoy a comfortable and laid-back stay in this 2-bedroom, 1-bathroom cottage just a short 5-minute walk from the beach.",
             facts=["1,000 sq ft", "2 Bedrooms, 1 Bathroom", "Fully equipped kitchen", "5-minute walk to the beach"],
             space=[("Living, Dining, and Kitchen",
                     "Cottage 12 at Ocean Spray Beach Resort features a spacious and inviting living room with an electric fireplace, sofa, and flat-screen TV, offering the perfect spot to unwind. The dining area comfortably seats six, ideal for shared meals. The fully equipped kitchen makes meal prep easy, adding to the charm of this great beach getaway!",
                     ["Spacious living room with electric fireplace", "Dining for 6", "Fully equipped kitchen"]),
                    ("Bedrooms and Bathroom",
                     "Relax in the two peaceful bedrooms at Cottage 12. The first features a comfortable queen-sized bed, while the second is perfect for families or groups with two sets of twin bunk beds. The bathroom offers a shower, along with essential toiletries to start your day refreshed.",
                     ["Queen bed in bedroom 1", "2 sets of twin bunk beds in bedroom 2", "1 Bathroom with shower"])],
             outdoor=None),
}


def title(n: int) -> str:
    return f"Affordable Grayland Beach Stay – Ocean Spray {n}"


def build(n: int, c: dict) -> docx.Document:
    unit = c.get("unit", "the cottage")
    d = docx.Document()
    st = d.styles["Normal"]
    st.font.name, st.font.size = "Arial", Pt(11)

    def head(text):
        d.add_paragraph().add_run(text).bold = True

    def para(text):
        d.add_paragraph(text)

    def ticks(items):
        for i in items:
            para(f"✔{i}")

    def numbered(items, start=1):
        for i, t in enumerate(items, start):
            para(f"{i}- {t}")
        return start + len(items)

    p = d.add_paragraph()
    p.add_run("Listing name: ").bold = True
    p.add_run(title(n)).bold = True

    head("Summary")
    para(" ".join([c["opener"], SUMMARY_SMALL if c["kind"] == "small" else SUMMARY_FAMILY, SUMMARY_CLOSE]))
    ticks(c["facts"])

    head("The Space")
    for sub, text, items in c["space"]:
        para(sub)
        para(text)
        ticks(items)
        para("")
    para("Outdoor Space")
    para(OUTDOOR)
    ticks(([c["outdoor"]] if c["outdoor"] else []) + OUTDOOR_BULLETS_TAIL)

    head("The Neighborhood")
    for t in NEIGHBORHOOD:
        para(t.format(unit=unit))

    head("Guest Access")
    para(ACCESS)

    head("Getting Around")
    para(TRANSIT)

    head("Other things to note")
    notes = ([c["first_note"]] if c.get("first_note") else []) + [NOTE_AC]
    notes += [NOTE_PETS, NOTE_OLDER, NOTE_KITCHEN, NOTE_COMMON]
    numbered(notes)

    head("House Rules")
    numbered(RULES_TOP)
    para(FINE)
    numbered([RULE_PETS, RULE_PRIVATE], start=5)
    k = numbered(RULES_BOTTOM, start=7)
    para(f"{k}- Check out procedure:")
    for t in CHECKOUT:
        para(f"- {t}")
    numbered(RULES_END, start=k + 1)
    return d


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    for n, c in C.items():
        path = OUT / f"Cottage {n} Listing Description.docx"
        build(n, c).save(path)
        summ = " ".join([c["opener"], SUMMARY_SMALL if c["kind"] == "small" else SUMMARY_FAMILY, SUMMARY_CLOSE])
        print(f"{path.name:42s} title {len(title(n))} ch, summary {len(summ)} ch")


if __name__ == "__main__":
    main()
