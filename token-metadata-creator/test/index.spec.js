const { execSync } = require('child_process');
const { readFileSync, writeFileSync } = require('fs');
const { mkdtemp } = require('fs/promises');
const { copySync } = require('fs-extra');
const os = require('os');
const path = require('path');
const { assert } = require('chai');

let cli, getDraft, withDraft, getFinal, writeTmpFile;


const alice = "19309eb9c066253cede617dc635223ace320ae0bbdd5bd1968439cd0";
const bob = "04c24626761279476a9da9b9d851328cab93d92e7a8790852e42ff894b746f725a436f696e";
const carole = "b78a3dd6ce08fc78cdff0aa8637a3e2a4527b91721180fd6728ae9d3";

const policies =
    { [alice]: {
        "type": "all",
        "scripts": [
            { "type": "sig", "keyHash": "2b0c33e73d2a70733edc971d19e2cafbada1692db2d35e7dc9453df2" }
        ]
      }
    , [bob]: {
        "type": "any",
        "scripts": [
            { "type": "sig", "keyHash": "8dd214875687235fb55f9bc011aeded6fc53c989be261b46c20df4d4" },
            { "type": "sig", "keyHash": "a8a511c306eb0fd3e503116f594e9beb2274a3c178b172d3ed2e42cf" },
        ]
      }
    , [carole]: {
        "type": "all",
        "scripts": [
            { "type": "sig", "keyHash": "8c322807654619d4e4ff2437c7d27db5e0610e11795233da82290a98" },
            { "type": "before", "slot": 42 },
        ]
      }
    }

const policyFiles =
    { [alice]: alice+".policy.json"
    , [bob]: bob+".policy.json"
    , [carole]: carole+".policy.json"
    }

const keys =
  { [alice]:
      [ `{ "type": "PaymentExtendedSigningKeyShelley_ed25519_bip32"
         , "description": "Payment Signing Key"
         , "cborHex": "5880807e4f00c6d7853c2c4e2da41d6047d1dded1ff4589ddcc8a6aadf71b13e6547ce73ced74b75236b3d9e143bb65104833ad3038b7739c82c4301d9ceb31ff7cdccce8b6a024f4c1d86bd44ed329885de5a6c8b73f45a407b66e030c13909f49a0c0250ec21e3c9c85c1d21fbf209a1b6610d3fa059b9ef48a68781e2d168b1ce"
         }`
      ]
  , [bob]:
      [ "addr_sk1pxpsyh9vac05ewq9eqey34zkvx06esedh6rlazdrja6epr7fmzfsdsyqag"
      , "0dc7a75e1337cb8f2f5902598f2dc1942a30d3c03333fc7d4834c3d2166718e2"
      ]
  , [carole]:
      [ ` { "type": "PaymentSigningKeyShelley_ed25519"
          , "description": "Payment Signing Key"
          , "cborHex": "5820a27a677bba62a1ab1934084fc6d820d59229a29a1d92401fd96a26ee6c072dd7"
          }`
      ]
  }

const keyFiles =
    { [alice]: [alice+".0.sk"]
    , [bob]: [bob+".0.sk", bob+".1.sk"]
    , [carole]: [carole+".0.sk"]
    }

describe("token-metadata-creator", () => {
  describe("Minimal workflow", () => {
    before(fixture);

    // NOTE
    // Tests belows aren't 'independent' but ought to be ran in sequence.

    it("--init", () => {
      cli(alice, "--init");
      assert.equal(getDraft(alice).subject, alice);
    });

    it("Add required fields", () => {
      const name = `ギル`;
      const description = `The currency in all of the Final Fantasy games.`;
      const policy = `82008201818200581c2b0c33e73d2a70733edc971d19e2cafbada1692db2d35e7dc9453df2`

      cli(alice, "--name", name, "--description", description, "--policy", policy);

      const empty = { sequenceNumber: 0, signatures: [] };
      assert.deepEqual(getDraft(alice).name, { ...empty, value: name });
      assert.deepEqual(getDraft(alice).description, { ...empty, value: description });
      assert.deepEqual(getDraft(alice).policy, policy);
    });


    it("Add optional fields", () => {
      const ticker = `GIL`;
      const url = `https://finalfantasy.fandom.com/wiki/Gil`;
      const logo = `testData/icon.png`
      const logoSerialized = "iVBORw0KGgoAAAANSUhEUgAAABkAAAAeCAYAAADZ7LXbAAAACXBIWXMAAA7EAAAOxAGVKw4bAAACbUlEQVRIie3Vy0tUURzA8e855965c8lXUhlhEQVBSEmQRAURQbSIEqFl4N6/oHYtAhdtonatK8hVBCERZC+0jbZpIRVkIeagTJrO3Nd5tBhDMHOcGiHCA2dxHvDh9zs/fkc45xwbPORGA5tI/RFdGCL9MgAm/mNEVKuuaHA3OW+RlDb8zjt4O07VjFRPV8NBZC5PGMxj3/YQv7uGs7p+iJ5+ipgfIZr7hnWSXBjgT98iHr6IS+fqg7h0Dl8ZQpmQFKdJSmWkkuSj10TD3WCzv0f89m6S8BjWQehbVDpPWiojsASlEeLxG3WIJFtANneQei3EqpnMeWRxgtMahYGP/dhoqiry2+rKJh9i3l8l2KIRUlVQazDlRXTpOzIr43uQ7LlCvrO/9kjisT7Ehz6CBgtCki4sEC+ALpdQQUC+qQmXC3EO3NQAsHaP/QVx1mBnh5BKYpOYON2L6npJ/sw4svMRacmCc+TyOQwKGX/CRl9rQ4SQyPZeFqM27L7bhCcHUY37AVCtR7EtZ8EZhLN4vkIKhy1N1Ibo4ijq83UavAl04QmIFVekB1aDNQhnQFBZ14KABauRaFThHrrwbPmkPImYeQw6A5OBNRjnIxsPrIl4KzdUcwep9SFL8JVHNnqJeFcvyBCm7hJQBKPBZJWH334eGe5cE1m1hKM3l8nP3kcICVLiEEuXLfycQKpBnnhRtWmuWsLBkZtEucNYa8BkCJMiTFrJ/RLgHJjWc+vqyqsiMthGePo5SWsP2ohKWpamdZBqQbz1AvnjD6oCsI7/RM+8whTHljf8RrzWLlTLoXUB60LqMf6NP34T+T+RH/HOKLJ+ho1iAAAAAElFTkSuQmCC"
      const decimals = 255;

      copyTestData();
      cli(alice, "--ticker", ticker, "--url", url, "--logo", logo, "--decimals", decimals);

      const empty = { sequenceNumber: 0, signatures: [] };
      assert.deepEqual(getDraft(alice).ticker, { ...empty, value: ticker });
      assert.deepEqual(getDraft(alice).url, { ...empty, value: url });
      assert.deepEqual(getDraft(alice).logo, { ...empty, value: logoSerialized });
      assert.deepEqual(getDraft(alice).decimals, { ...empty, value: decimals});
    });

    it("No other fields are supported!", () => {
      const unit = `2,cents`;
      try {
        cli(alice, "--unit ", unit);
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Validate after signing", () => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--finalize");
      assert.lengthOf(getFinal(alice).name.signatures, 1);
      assert.lengthOf(getFinal(alice).description.signatures, 1);
    });
  });

  describe("Draft editing", () => {
    before(fixture);
    beforeEach(() => cli(alice, "--init"));

    it("Reset content on --init", () => {
      cli(alice, "--name", "foo");
      assert.isNotNull(getDraft(alice).name);
      cli(alice, "--init");
      assert.isNull(getDraft(alice).name);
    });

    it("Edit property on successive calls", () => {
      let name = "SuperCoin"
      cli(alice, "--name", "foo");
      cli(alice, "--name", "bar");
      cli(alice, "--name", name);
      assert.equal(getDraft(alice).name.value, name);
      assert.equal(getDraft(alice).name.sequenceNumber, 2);
    });

    it("Remove signatures on edit to different value", () => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      cli(alice, "--name", "foo")
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--name", "bar")
      assert.equal(getDraft(alice).name.value, "bar");
      assert.lengthOf(getDraft(alice).name.signatures, 0);
      assert.equal(getDraft(alice).name.sequenceNumber, 1);
    });

    it("Keep signatures on edit to same value", () => {
      writeTmpFile(keyFiles[alice][0], keys[alice][0]);
      cli(alice, "--name", "foo")
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--name", "foo")
      assert.equal(getDraft(alice).name.value, "foo");
      assert.lengthOf(getDraft(alice).name.signatures, 1);
      assert.equal(getDraft(alice).name.sequenceNumber, 0);
    });
  });

  describe("Signing", () => {
    before(fixture);
    beforeEach(() => {
      cli(alice, "--init");
      cli(alice, "--name", "foo", "--description", "lorem ipsum");
      cli(alice, "--policy", policyFiles[alice]);

      cli(bob, "--init");
      cli(bob, "--name", "foo", "--description", "lorem ipsum");
      cli(bob, "--policy", policyFiles[bob]);
    });

    it("Alice can attest her metadata", () => {
      cli(alice, "-a", keyFiles[alice][0]);
      cli(alice, "--finalize");
    });

    it("Bob can attest his metadata with key #0", () => {
      cli(bob, "-a", keyFiles[bob][0]);
      cli(bob, "--finalize");
    });

    it("Bob can attest his metadata with key #1", () => {
      cli(bob, "-a", keyFiles[bob][1]);
      cli(bob, "--finalize");
    });

    it("Bob can attest his metadata with keys #0 & #1", () => {
      cli(bob, "-a", keyFiles[bob][0]);
      cli(bob, "-a", keyFiles[bob][1]);
      cli(bob, "--finalize");
    });

    it("Can't finalize without attesting", () => {
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Can't finalize without attesting ALL properties", () => {
      cli(alice, "--attest-name", "-a", keyFiles[alice][0]);
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Keys from Bob don't attest for metadata of Alice", () => {
      cli(alice, "-a", keyFiles[bob][0]);
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Old signatures don't attest for new metadata", () => {
      cli(alice, "-a", keyFiles[alice][0]);
      const old = getDraft(alice);
      cli(alice, "--name", "bar");
      withDraft(alice, draft => {
        draft.name.signatures = old.name.signatures;
        return draft;
      });
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Signatures of an property don't work for another", () => {
      cli(alice, "--attest-name", "-a", keyFiles[alice][0]);
      withDraft(alice, draft => {
          draft.description.signatures = draft.name.signatures;
          return draft;
      });
      try {
        cli(alice, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });

    it("Bob can't use Alice's policy", () => {
      cli(bob, "--policy", policyFiles[alice]);
      cli(bob, "-a", keyFiles[bob][0]);
      try {
        cli(bob, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });
  });


  describe("Time Constraints", () => {
    before(fixture);

    it("Carole can attest even with expired time constraints", () => {
      cli(carole, "--init");
      cli(carole, "--name", "foo", "--description", "lorem ipsum");
      cli(carole, "--policy", policyFiles[carole]);
      cli(carole, "-a", keyFiles[carole][0]);
      cli(carole, "--finalize");
    });

    it("Alice can't attest for Carole", () => {
      cli(carole, "--init");
      cli(carole, "--name", "foo", "--description", "lorem ipsum");
      cli(carole, "--policy", policyFiles[carole]);
      cli(carole, "-a", keyFiles[alice][0]);
      try {
        cli(carole, "--finalize");
        assert.fail("should have thrown.");
      } catch (e) {}
    });
  });
});

function rawOrQuoted(arg) {
  if ((typeof arg) == "string") {
    return arg.startsWith("-") ? arg : `"${arg}"`;
  } else {
    return arg;
  }
}

function fixture(done) {
  mkdtemp(path.join(os.tmpdir(), "token-metadata-creator"))
    .then(cwd => {
      copyTestData = function copyTestData() {
        copySync(path.join(__dirname, "testData"), path.join(cwd, "testData"))
      };

      cli = function cli(subject, ...args) {
        const prepared = args.map(arg => rawOrQuoted(arg));
        const opts = { cwd, stdio: 'ignore'  }
        // const opts = { cwd }
        return execSync(`token-metadata-creator entry ${subject} ${prepared.join(" ")}`, opts);
      };

      getDraft = function getDraft(subject) {
        return JSON.parse(readFileSync(path.join(cwd, subject + ".json.draft")));
      }

      withDraft = function writeDraft(subject, fn) {
        const draft = getDraft(subject)
        writeFileSync(path.join(cwd, subject + ".json.draft"), JSON.stringify(fn(draft)));
      }

      getFinal = function getFinal(subject) {
        return JSON.parse(readFileSync(path.join(cwd, subject + ".json")));
      }

      writeTmpFile = function writeTmpFile(filename, buffer) {
        writeFileSync(path.join(cwd, filename), buffer);
      }

      // Make all known keys and policies available in the tmp state directory.

      Object.getOwnPropertyNames(keys).forEach(subject => {
        keys[subject].forEach((k, i) => {
          writeTmpFile(keyFiles[subject][i], keys[subject][i])
        });
      });

      Object.getOwnPropertyNames(policies).forEach(subject => {
        writeTmpFile(policyFiles[subject], JSON.stringify(policies[subject]));
      });
    })
    .then(done)
    .catch(console.fatal);
}
