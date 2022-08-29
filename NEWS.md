# HTI.MSNA.2022 0.0.2.9000

* Update data.
* Font size is now 10px for `mod_graph_main_server/output/graph`.
* `ggplot_to_plotly` does not autosize yaxis.
* Remove download table from `mod_indicator_main_server`
* `mod_indicator` now follows a sidebarPane layout.

---

# HTI.MSNA.2022 0.0.1.9000

* Added a `NEWS.md` and a `README.md` files to track changes to the package and to explicit its use and aim (#3)
* The new helper `mutate_if_nulla()`  is added to `fct_helpers.R` and covers all mods to mutate one column to itself while replacing `NA` or `NULL` values by a `replacement` argument. (#2)
* Bug fix: Font size is now 10px for `mod_indicator_main_server/output/table`. (#1)
* Many many changes since first commit 0.0.0 version. Far too long to explain.
