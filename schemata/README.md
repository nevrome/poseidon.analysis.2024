convert .svg to .pdf:

```bash
inkscape -d 300 --export-area-drawing interaction_schema/interaction_schema.svg -o interaction_schema/interaction_schema.pdf
inkscape -d 300 --export-area-drawing overview_and_package_by_michelle_oreilly/schema_overview.svg -o overview_and_package_by_michelle_oreilly/schema_overview.pdf
inkscape -d 300 --export-area-drawing overview_and_package_by_michelle_oreilly/schema_package.svg -o overview_and_package_by_michelle_oreilly/schema_package.pdf
```

convert .svg to .png:

```bash
convert poseidon_overview.svg -background none -size 2048x2048 poseidon_overview.png
```

