import base64
import time
from urllib.parse import urlencode
import requests
from .config import env
from .utils import read_json, write_json

TOKEN_PATH_DEFAULT = "./data/qbo_tokens.json"

class QBOClient:
    def __init__(self, realm_id: str, base_url: str, minorversion: int = 75, token_path: str = TOKEN_PATH_DEFAULT):
        self.realm_id = realm_id
        self.base_url = base_url.rstrip("/")
        self.minorversion = minorversion
        self.token_path = token_path

        self.client_id = env("QBO_CLIENT_ID")
        self.client_secret = env("QBO_CLIENT_SECRET")
        self.redirect_uri = env("QBO_REDIRECT_URI", "http://localhost:8000/callback")

        if not self.client_id or not self.client_secret:
            raise RuntimeError("Missing QBO_CLIENT_ID or QBO_CLIENT_SECRET in environment (.env).")

    def _basic_auth_header(self) -> str:
        tok = f"{self.client_id}:{self.client_secret}".encode("utf-8")
        return "Basic " + base64.b64encode(tok).decode("utf-8")

    def auth_url(self, state: str) -> str:
        params = {
            "client_id": self.client_id,
            "scope": "com.intuit.quickbooks.accounting",
            "redirect_uri": self.redirect_uri,
            "response_type": "code",
            "state": state,
        }
        return "https://appcenter.intuit.com/connect/oauth2?" + urlencode(params)

    def exchange_code_for_tokens(self, code: str) -> dict:
        url = "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer"
        headers = {
            "Authorization": self._basic_auth_header(),
            "Accept": "application/json",
            "Content-Type": "application/x-www-form-urlencoded",
        }
        data = {"grant_type": "authorization_code", "code": code, "redirect_uri": self.redirect_uri}
        r = requests.post(url, headers=headers, data=data, timeout=60)
        r.raise_for_status()
        tokens = r.json()
        tokens["_obtained_at"] = int(time.time())
        write_json(self.token_path, tokens)
        return tokens

    def refresh_tokens(self) -> dict:
        tokens = read_json(self.token_path, default=None)
        if not tokens or "refresh_token" not in tokens:
            raise RuntimeError("No refresh_token found. Run OAuth flow first.")
        url = "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer"
        headers = {
            "Authorization": self._basic_auth_header(),
            "Accept": "application/json",
            "Content-Type": "application/x-www-form-urlencoded",
        }
        data = {"grant_type": "refresh_token", "refresh_token": tokens["refresh_token"]}
        r = requests.post(url, headers=headers, data=data, timeout=60)
        print("STATUS:", r.status_code)
        print("BODY:", r.text)
        r.raise_for_status()
        newt = r.json()
        newt["_obtained_at"] = int(time.time())
        if "refresh_token" not in newt:
            newt["refresh_token"] = tokens["refresh_token"]
        write_json(self.token_path, newt)
        return newt

    def _get_access_token(self) -> str:
        tokens = read_json(self.token_path, default=None)
        if not tokens:
            raise RuntimeError("No tokens found. Run OAuth flow first.")
        obtained = int(tokens.get("_obtained_at", 0))
        expires_in = int(tokens.get("expires_in", 3600))
        now = int(time.time())
        if now > obtained + expires_in - 120:
            tokens = self.refresh_tokens()
        return tokens["access_token"]

    def request(self, method: str, path: str, params: dict | None = None, json_body: dict | None = None) -> dict:
        token = self._get_access_token()
        headers = {"Authorization": f"Bearer {token}", "Accept": "application/json", "Content-Type": "application/json"}
        params = params or {}
        params["minorversion"] = self.minorversion
        url = f"{self.base_url}{path}"
        r = requests.request(method, url, headers=headers, params=params, json=json_body, timeout=90)
        r.raise_for_status()
        return r.json()

    def query(self, q: str, start_position: int = 1, max_results: int = 1000) -> dict:
        # QBO expects STARTPOSITION / MAXRESULTS inside the query string
        q_clean = q.strip().rstrip(";")
        q_paged = f"{q_clean} STARTPOSITION {int(start_position)} MAXRESULTS {int(max_results)}"

        print(f"QBO query: {q_paged}")

        path = f"/v3/company/{self.realm_id}/query"
        params = {
            "query": q_paged,
        }
        return self.request("GET", path, params=params)