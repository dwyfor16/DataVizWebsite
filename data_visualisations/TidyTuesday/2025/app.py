
import pandas as pd
import plotly.express as px
from shiny import App, ui, render, reactive

# UI layout
app_ui = ui.page_fluid(
    ui.panel_title("Mount Vesuvius Earthquake Explorer"),
    
    ui.input_slider("year_range", "Select Year Range", 1999, 2025, value=(2020, 2024)),
    
    ui.output_plot("quake_map")
)

# Server logic
def server(input, output, session):
    @reactive.Calc
    def cleaned_data():
        # Load data
        url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv'
        df = pd.read_csv(url)

        # Data cleaning
        df = df[df['review_level'] != "revised"]
        df = df.dropna(subset=['latitude', 'longitude', 'duration_magnitude_md'])
        df = df[df['duration_magnitude_md'] > 0.3]
        df['time'] = pd.to_datetime(df['time'])
        df['date'] = df['time'].dt.date
        df['year'] = df['time'].dt.year
        
        # Filter based on input
        df = df[(df['year'] >= input.year_range()[0]) & (df['year'] <= input.year_range()[1])]
        return df

    @output
    @render.plot
    def quake_map():
        df = cleaned_data()
        
        if df.empty:
            return px.scatter_mapbox(title="No data available for selected year range")

        fig = px.scatter_mapbox(
            df,
            lat="latitude",
            lon="longitude",
            color="duration_magnitude_md",
            size="duration_magnitude_md",
            animation_frame="date",
            color_continuous_scale="Viridis",
            size_max=15,
            zoom=10,
            center={"lat": 40.821, "lon": 14.426},
            mapbox_style="carto-positron",
            title="Earthquake Occurrences in the Vicinity of Mount Vesuvius Over Time"
        )

        fig.update_coloraxes(colorbar_title="Magnitude Scale")

        # Faster animation
        fig.layout.updatemenus[0].buttons[0].args[1]["frame"]["duration"] = 100
        fig.layout.updatemenus[0].buttons[0].args[1]["transition"]["duration"] = 0

        return fig

# Launch app
app = App(app_ui, server)


