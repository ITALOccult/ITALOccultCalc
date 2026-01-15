using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ListPlottedObservers : Form
	{
		private IContainer components;

		private Label label2;

		private ListBox lstObservers;

		public ListPlottedObservers()
		{
			InitializeComponent();
		}

		private void lstObservers_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			if (((ListControl)lstObservers).get_SelectedIndex() >= 0)
			{
				string text = lstObservers.get_Items().get_Item(((ListControl)lstObservers).get_SelectedIndex()).ToString()!.Substring(0, 20);
				if (text.Length >= 1)
				{
					ReductionProfile.PlotReductionProfile(HighlightRePlot: true, "", text, "", ExcludeLargeResiduals: false, IncludeStarPath: false, IncludeAllPaths: false, ReReadHiresData: false);
				}
			}
		}

		private void ListPlottedObservers_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			ListPlottedObservers_Activated(sender, e);
		}

		private void ListPlottedObservers_Activated(object sender, EventArgs e)
		{
			lstObservers.get_Items().Clear();
			for (int i = 0; i < ReductionProfile.ObserverList.Count; i++)
			{
				lstObservers.get_Items().Add((object)ReductionProfile.ObserverList[i].ToString());
			}
			lstObservers.set_Sorted(true);
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
			//IL_0127: Unknown result type (might be due to invalid IL or missing references)
			//IL_0131: Expected O, but got Unknown
			//IL_019b: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a5: Expected O, but got Unknown
			label2 = new Label();
			lstObservers = new ListBox();
			((Control)this).SuspendLayout();
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(20, 5));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(192, 12));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("double-click to highlight Observer in the profile");
			((Control)lstObservers).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstObservers).set_FormattingEnabled(true);
			lstObservers.set_ItemHeight(14);
			((Control)lstObservers).set_Location(new Point(7, 20));
			((Control)lstObservers).set_Name("lstObservers");
			((Control)lstObservers).set_Size(new Size(222, 466));
			((Control)lstObservers).set_TabIndex(6);
			((Control)lstObservers).add_MouseDoubleClick(new MouseEventHandler(lstObservers_MouseDoubleClick));
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(238, 490));
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)lstObservers);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarListPlottedObservers", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarListPlottedObservers);
			((Control)this).set_Name("ListPlottedObservers");
			((Control)this).set_Text("ListPlottedObservers");
			((Form)this).add_Load((EventHandler)ListPlottedObservers_Load);
			((Form)this).add_Activated((EventHandler)ListPlottedObservers_Activated);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
