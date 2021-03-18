from random import choice
from string import ascii_uppercase

subjects = ["01fb761b09aec85a63fb742c4dab2b72499bca6a6006b7594de6cb95",
            "01fb761b09aec85a63fb742c4dab2b72499bca6a6006b7594de6cb96",
            "19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0",
            "1a3e62dd2d0d38cc6ccb05801a4baff13126fc54bab5e6424469783031",
            "1a3e62dd2d0d38cc6ccb05801a4baff13126fc54bab5e64244697830",
            "1e80fa1859c59b18ff4895a2c481cced459c6b4fcd6c445b5e907a92626967636f696e",
            "2e6d83507419c027eac6eb9430b780c0793e2c1762b7bff56fbba6f5736f6d65636f696e",
            "34250edd1e9836f5378702fbf9416b709bc140e04f668cc3552085184154414441636f696e",
            "34250edd1e9836f5378702fbf9416b709bc140e04f668cc355208518",
            "3e8777fa3ed835dd8036d2182918845c72a91b22c098439fd77dcbc350",
            "446ffdf62c2474c0f84387b3ed0796b7d2daab0b282742dfad02da3f544553544e45544e465430303076303030",
            "4bfe7acae1bd2599649962b146a1e47d2e14933809b367e804c61f86",
            "69b30e43bc5401bb34d0b12bd06cd9b537f33065aa49df7e8652739d4c51",
            "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7",
            "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f16861707079636f696e",
            "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f17375706572636f696e",
            "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f174727565636f696e",
            "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1",
            "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0",
            "94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c676f6f64636f696e",
            "94d4cdbcffb09ebd4780d94f932a657dc4852530fa8013df66c72d4c",
            "9b87fdecd7a27e3b570fe821d790034a782d5248f4d22efc165c4d20796f7572636f696e",
            "baa836fef09cb35e180fce4b55ded152907af1e2c840ed5218776f2f",
            "c43a140cfe4476635776dbc5adea18604997348fc00607925165d4d4636f6c696e636f696e",
            "c43a140cfe4476635776dbc5adea18604997348fc00607925165d4d46a616d6573636f696e",
            "caa836fef09cb35e180fce4b55ded152907af1e2c840ed5218776f2f",
            "daa836fef09cb35e180fce4b55ded152907af1e2c840ed5218776f2f",
            "fc88532b68d36f5d951f44155166091a234a064250a6ae844128f8a4686f736b636f696e73",
            "mynewsubject-is-very-long-256-characters-in-fact-there-is-a-long-story-id-like-to-fit-here-but-cant-quite-there-is-a-long-story-id-like-to-fit-here-but-cant-quite-there-is-a-long-story"
            ]

def get_random_subject():
    return choice(subjects)

def get_invalid_subject():
    return (''.join(choice(ascii_uppercase) for i in range(56)))

if __name__ == '__main__':
    pass
