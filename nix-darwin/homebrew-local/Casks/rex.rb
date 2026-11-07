cask "rex" do
  version "0.1.0,1853"
  sha256 "e2e5c4021dfe9d9b6265dd1a641c6fc733a9b25215914572ea780e0b3013b0a5"

  url "https://blob.superlogical.com/public/rex-app/preview/Rex-#{version.csv.first}-#{version.csv.second}.dmg"
  name "Rex"
  desc "Superlogical's multiplexer for everything (beta)"
  homepage "https://www.superlogical.com/"

  livecheck do
    url "https://blob.superlogical.com/public/rex-app/preview/appcast.xml"
    strategy :sparkle
  end

  auto_updates true
  depends_on macos: :sequoia

  app "Rex Beta.app"
end
