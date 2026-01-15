using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class ShapeModelFits : Form
	{
		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withShapeModelFitsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byEventDateToolStripMenuItem;

		private ToolStripMenuItem byShapeModelToolStripMenuItem;

		private ToolStripMenuItem byMaximumDiameterToolStripMenuItem;

		private ListBox lstAsteroids;

		private ListBox lstFits;

		private ToolStripMenuItem sortAsteroidListToolStripMenuItem;

		private ToolStripMenuItem byNumToolStripMenuItem;

		private ToolStripMenuItem byNameToolStripMenuItem;

		private ToolStripMenuItem byEventsToolStripMenuItem;

		private ToolStripMenuItem byMinimumDiameterToolStripMenuItem;

		private ToolStripMenuItem byQualityOfTheFitToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem copyListOfAsteroidsToolStripMenuItem;

		public ShapeModelFits()
		{
			InitializeComponent();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(), "Shape model fits for ", Settings.Default.Save_AsteroidResults);
		}

		private void byEventDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListSolutions(0);
		}

		private void byShapeModelToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListSolutions(1);
		}

		private void byMaximumDiameterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListSolutions(2);
		}

		private void byMinimumDiameterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListSolutions(3);
		}

		private void byQualityOfTheFitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListSolutions(4);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstFits.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstFits.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		public void Display()
		{
			lstFits.get_Items().Clear();
			lstFits.get_Items().Add((object)"Shape model fits for ");
			lstFits.get_Items().Add((object)"");
			for (int i = 0; i < Asteroid_Observations_Reports.BinaryList.Count; i++)
			{
				for (int j = 0; j < Asteroid_Observations_Reports.BinaryList.Count; j++)
				{
					lstFits.get_Items().Add((object)Asteroid_Observations_Reports.BinaryList[i].ToString());
				}
			}
		}

		internal void ListAsteroids()
		{
			lstAsteroids.get_Items().Clear();
			for (int i = 0; i < Asteroid_Observations_Reports.AsteroidsHavingShapes.Count; i++)
			{
				if (i > 0 && i % 5 == 0)
				{
					lstAsteroids.get_Items().Add((object)"");
				}
				lstAsteroids.get_Items().Add((object)Asteroid_Observations_Reports.AsteroidsHavingShapes[i].ToString());
			}
		}

		private void byNumToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidsWithShapes.SortField = 0;
			Asteroid_Observations_Reports.AsteroidsHavingShapes.Sort();
			ListAsteroids();
		}

		private void byNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidsWithShapes.SortField = 1;
			Asteroid_Observations_Reports.AsteroidsHavingShapes.Sort();
			ListAsteroids();
		}

		private void byEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidsWithShapes.SortField = 2;
			Asteroid_Observations_Reports.AsteroidsHavingShapes.Sort();
			ListAsteroids();
		}

		private void ShapeModelFits_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 500)
			{
				((Control)this).set_Width(500);
			}
			if (((Control)this).get_Height() < 300)
			{
				((Control)this).set_Height(300);
			}
			ListBox obj = lstAsteroids;
			int height;
			((Control)lstFits).set_Height(height = ((Control)this).get_Height() - 100);
			((Control)obj).set_Height(height);
			((Control)lstFits).set_Width(((Control)this).get_Width() - 220);
		}

		private void lstAsteroids_DoubleClick(object sender, EventArgs e)
		{
			if (lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Trim().Length >= 14)
			{
				string asteroidID = lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(7, 14).Trim();
				Data_and_Plots.Historical_AllEvents.GetShapeModels(asteroidID);
				ListSolutions(4);
			}
		}

		internal void ListSolutions(int SortField)
		{
			int.TryParse(lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(0, 6).Trim(), out var result);
			Utilities.Display_IR_AsteroidDiameter(result, ShowInForm: false, out var Diameters);
			string[] array = Diameters.Split(new char[1] { '\r' });
			string[] array2 = new string[5] { "event date", "shape model", "maximum volume-equivalent diameter", "minimum volume-equivalent diameter", "the quality of the shape model fit" };
			ShapeModels.SortField = SortField;
			Asteroid_Observations_Reports.ShapesList.Sort();
			lstFits.get_Items().Clear();
			lstFits.get_Items().Add((object)("Satellite IR diameters for (" + lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(0, 6).Trim() + ") " + lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(7, 14).Trim()));
			for (int i = 2; i < array.Length; i++)
			{
				lstFits.get_Items().Add((object)array[i].Replace('\n', ' '));
			}
			lstFits.get_Items().Add((object)"".PadRight(70, '_'));
			lstFits.get_Items().Add((object)"");
			lstFits.get_Items().Add((object)("Shape model fits to (" + lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(0, 6).Trim() + ") " + lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(7, 14).Trim()));
			lstFits.get_Items().Add((object)("Sorted by " + array2[SortField]));
			lstFits.get_Items().Add((object)"");
			switch (SortField)
			{
			case 0:
				lstFits.get_Items().Add((object)"  Model        Min  Max   phase  Fit");
				break;
			case 1:
				lstFits.get_Items().Add((object)"   Date      Min  Max   phase  Fit");
				break;
			default:
				lstFits.get_Items().Add((object)"   Date        Model        Min  Max   phase  Fit");
				break;
			}
			lstFits.get_Items().Add((object)"");
			for (int j = 0; j < Asteroid_Observations_Reports.ShapesList.Count; j++)
			{
				switch (SortField)
				{
				case 0:
					if (j == 0)
					{
						lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].EventDate());
					}
					if (j > 0 && Asteroid_Observations_Reports.ShapesList[j].EventDate() != Asteroid_Observations_Reports.ShapesList[j - 1].EventDate())
					{
						lstFits.get_Items().Add((object)"");
						lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].EventDate());
					}
					lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].SolutionLine_WithModel());
					continue;
				case 1:
					if (j == 0)
					{
						lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].ShapeModelDetails());
					}
					if (j > 0 && ((Asteroid_Observations_Reports.ShapesList[j].Source != Asteroid_Observations_Reports.ShapesList[j - 1].Source) | (Asteroid_Observations_Reports.ShapesList[j].ID != Asteroid_Observations_Reports.ShapesList[j - 1].ID)))
					{
						lstFits.get_Items().Add((object)"");
						lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].ShapeModelDetails());
					}
					lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].SolutionLine_NoModel());
					continue;
				}
				if (SortField == 2 || SortField == 3)
				{
					if (j > 0)
					{
						if (SortField == 2 && ((Asteroid_Observations_Reports.ShapesList[j].VolumeDia_Max == 0.0) & (Asteroid_Observations_Reports.ShapesList[j - 1].VolumeDia_Max > 0.0)))
						{
							lstFits.get_Items().Add((object)"");
							lstFits.get_Items().Add((object)"");
						}
						if (SortField == 3 && ((Asteroid_Observations_Reports.ShapesList[j].VolumeDia_Min == 0.0) & (Asteroid_Observations_Reports.ShapesList[j - 1].VolumeDia_Min > 0.0)))
						{
							lstFits.get_Items().Add((object)"");
							lstFits.get_Items().Add((object)"");
						}
					}
					lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].SolutionLine_WithDateModel());
				}
				else if (SortField == 4)
				{
					if (j == 0)
					{
						lstFits.get_Items().Add((object)ShapeModels.Quality[Asteroid_Observations_Reports.ShapesList[j].FitQuality]);
					}
					if (j > 0 && Asteroid_Observations_Reports.ShapesList[j].FitQuality != Asteroid_Observations_Reports.ShapesList[j - 1].FitQuality)
					{
						lstFits.get_Items().Add((object)"");
						lstFits.get_Items().Add((object)ShapeModels.Quality[Asteroid_Observations_Reports.ShapesList[j].FitQuality]);
					}
					lstFits.get_Items().Add((object)Asteroid_Observations_Reports.ShapesList[j].SolutionLine_WithDateModel());
				}
			}
		}

		private void lstAsteroids_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Trim().Length >= 14)
			{
				string asteroidID = lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString()!.Substring(7, 14).Trim();
				Data_and_Plots.Historical_AllEvents.GetShapeModels(asteroidID);
				ListSolutions(4);
			}
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			foreach (object selectedItem in lstFits.get_SelectedItems())
			{
				text = text + selectedItem.ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void copyListOfAsteroidsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			int num = 0;
			for (int i = 0; i < lstAsteroids.get_Items().get_Count(); i++)
			{
				string text2 = lstAsteroids.get_Items().get_Item(i).ToString();
				if (text2.Trim().Length > 2)
				{
					num++;
				}
				text = text + text2 + "\r\n";
			}
			Clipboard.SetText(string.Format(num + " Asteroids fitted to shape models\r\n\r\nNumber Name        Events\r\n") + text);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withShapeModelFitsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyListOfAsteroidsToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortAsteroidListToolStripMenuItem = new ToolStripMenuItem();
			byNumToolStripMenuItem = new ToolStripMenuItem();
			byNameToolStripMenuItem = new ToolStripMenuItem();
			byEventsToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byEventDateToolStripMenuItem = new ToolStripMenuItem();
			byShapeModelToolStripMenuItem = new ToolStripMenuItem();
			byMaximumDiameterToolStripMenuItem = new ToolStripMenuItem();
			byMinimumDiameterToolStripMenuItem = new ToolStripMenuItem();
			byQualityOfTheFitToolStripMenuItem = new ToolStripMenuItem();
			lstAsteroids = new ListBox();
			lstFits = new ListBox();
			label1 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withShapeModelFitsToolStripMenuItem,
				(ToolStripItem)sortAsteroidListToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(647, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withShapeModelFitsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)copyListOfAsteroidsToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withShapeModelFitsToolStripMenuItem).set_Name("withShapeModelFitsToolStripMenuItem");
			((ToolStripItem)withShapeModelFitsToolStripMenuItem).set_Size(new Size(157, 20));
			((ToolStripItem)withShapeModelFitsToolStripMenuItem).set_Text("with Shape model fits....    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(184, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy All");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(184, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy Selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyListOfAsteroidsToolStripMenuItem).set_Name("copyListOfAsteroidsToolStripMenuItem");
			((ToolStripItem)copyListOfAsteroidsToolStripMenuItem).set_Size(new Size(184, 22));
			((ToolStripItem)copyListOfAsteroidsToolStripMenuItem).set_Text("Copy list of asteroids");
			((ToolStripItem)copyListOfAsteroidsToolStripMenuItem).add_Click((EventHandler)copyListOfAsteroidsToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(184, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(184, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(184, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortAsteroidListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)byNumToolStripMenuItem,
				(ToolStripItem)byNameToolStripMenuItem,
				(ToolStripItem)byEventsToolStripMenuItem
			});
			((ToolStripItem)sortAsteroidListToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortAsteroidListToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortAsteroidListToolStripMenuItem).set_Name("sortAsteroidListToolStripMenuItem");
			((ToolStripItem)sortAsteroidListToolStripMenuItem).set_Size(new Size(140, 20));
			((ToolStripItem)sortAsteroidListToolStripMenuItem).set_Text("Sort asteroid list...    ");
			((ToolStripItem)byNumToolStripMenuItem).set_Name("byNumToolStripMenuItem");
			((ToolStripItem)byNumToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)byNumToolStripMenuItem).set_Text("by number");
			((ToolStripItem)byNumToolStripMenuItem).add_Click((EventHandler)byNumToolStripMenuItem_Click);
			((ToolStripItem)byNameToolStripMenuItem).set_Name("byNameToolStripMenuItem");
			((ToolStripItem)byNameToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)byNameToolStripMenuItem).set_Text("by name");
			((ToolStripItem)byNameToolStripMenuItem).add_Click((EventHandler)byNameToolStripMenuItem_Click);
			((ToolStripItem)byEventsToolStripMenuItem).set_Name("byEventsToolStripMenuItem");
			((ToolStripItem)byEventsToolStripMenuItem).set_Size(new Size(134, 22));
			((ToolStripItem)byEventsToolStripMenuItem).set_Text("by # events");
			((ToolStripItem)byEventsToolStripMenuItem).add_Click((EventHandler)byEventsToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)byEventDateToolStripMenuItem,
				(ToolStripItem)byShapeModelToolStripMenuItem,
				(ToolStripItem)byMaximumDiameterToolStripMenuItem,
				(ToolStripItem)byMinimumDiameterToolStripMenuItem,
				(ToolStripItem)byQualityOfTheFitToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(155, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort shape model fits...");
			((ToolStripItem)byEventDateToolStripMenuItem).set_Name("byEventDateToolStripMenuItem");
			((ToolStripItem)byEventDateToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)byEventDateToolStripMenuItem).set_Text("by Event date");
			((ToolStripItem)byEventDateToolStripMenuItem).add_Click((EventHandler)byEventDateToolStripMenuItem_Click);
			((ToolStripItem)byShapeModelToolStripMenuItem).set_Name("byShapeModelToolStripMenuItem");
			((ToolStripItem)byShapeModelToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)byShapeModelToolStripMenuItem).set_Text("by Shape model");
			((ToolStripItem)byShapeModelToolStripMenuItem).add_Click((EventHandler)byShapeModelToolStripMenuItem_Click);
			((ToolStripItem)byMaximumDiameterToolStripMenuItem).set_Name("byMaximumDiameterToolStripMenuItem");
			((ToolStripItem)byMaximumDiameterToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)byMaximumDiameterToolStripMenuItem).set_Text("by Maximum diameter");
			((ToolStripItem)byMaximumDiameterToolStripMenuItem).add_Click((EventHandler)byMaximumDiameterToolStripMenuItem_Click);
			((ToolStripItem)byMinimumDiameterToolStripMenuItem).set_Name("byMinimumDiameterToolStripMenuItem");
			((ToolStripItem)byMinimumDiameterToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)byMinimumDiameterToolStripMenuItem).set_Text("by Minimum diameter");
			((ToolStripItem)byMinimumDiameterToolStripMenuItem).add_Click((EventHandler)byMinimumDiameterToolStripMenuItem_Click);
			((ToolStripItem)byQualityOfTheFitToolStripMenuItem).set_Name("byQualityOfTheFitToolStripMenuItem");
			((ToolStripItem)byQualityOfTheFitToolStripMenuItem).set_Size(new Size(195, 22));
			((ToolStripItem)byQualityOfTheFitToolStripMenuItem).set_Text("by Quality of the fit");
			((ToolStripItem)byQualityOfTheFitToolStripMenuItem).add_Click((EventHandler)byQualityOfTheFitToolStripMenuItem_Click);
			((Control)lstAsteroids).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstAsteroids).set_FormattingEnabled(true);
			((Control)lstAsteroids).set_Location(new Point(8, 45));
			((Control)lstAsteroids).set_Name("lstAsteroids");
			((Control)lstAsteroids).set_Size(new Size(178, 459));
			((Control)lstAsteroids).set_TabIndex(1);
			lstAsteroids.add_SelectedIndexChanged((EventHandler)lstAsteroids_SelectedIndexChanged);
			((Control)lstAsteroids).add_DoubleClick((EventHandler)lstAsteroids_DoubleClick);
			((Control)lstFits).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstFits).set_FormattingEnabled(true);
			((Control)lstFits).set_Location(new Point(198, 45));
			((Control)lstFits).set_Name("lstFits");
			lstFits.set_SelectionMode((SelectionMode)3);
			((Control)lstFits).set_Size(new Size(442, 459));
			((Control)lstFits).set_TabIndex(2);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(15, 32));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(157, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Number Name        Events");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(647, 520));
			((Control)this).get_Controls().Add((Control)(object)lstFits);
			((Control)this).get_Controls().Add((Control)(object)lstAsteroids);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("ShapeModelFits");
			((Control)this).set_Text("ShapeModelFits");
			((Control)this).add_Resize((EventHandler)ShapeModelFits_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
