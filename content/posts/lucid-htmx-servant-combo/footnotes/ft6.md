I sometimes still use Lucid v1 for backwards compatibility, e.g., in this very website, which is needed for compatibility with h`lucid-svg` (as far as I remember).

If you want to reuse my `Main.hs` function with Lucid v1, just replace `import qualified Servant.API.ContentTypes.Lucid as SL` with `import qualified Servant.HTML.Lucid as SL`.