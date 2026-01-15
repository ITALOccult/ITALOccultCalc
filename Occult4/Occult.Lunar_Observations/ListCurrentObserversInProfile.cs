using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ListCurrentObserversInProfile : Form
	{
		private IContainer components;

		private ListBox lstObservers;

		private Label label1;

		private CheckBox chkStarPath;

		private CheckBox chkAll;

		public ListCurrentObserversInProfile()
		{
			InitializeComponent();
		}

		private void ListCurrentObserversInProfile_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			ListCurrentObserversInProfile_Activated(sender, e);
		}

		private void lstObservers_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			if (((ListControl)lstObservers).get_SelectedIndex() >= 0)
			{
				string text = lstObservers.get_Items().get_Item(((ListControl)lstObservers).get_SelectedIndex()).ToString();
				if (text.Length >= 1)
				{
					ReductionProfile.PlotReductionProfile(HighlightRePlot: true, "", text, "", ExcludeLargeResiduals: false, chkStarPath.get_Checked(), chkAll.get_Checked(), ReReadHiresData: false);
				}
			}
		}

		private void ListCurrentObserversInProfile_Activated(object sender, EventArgs e)
		{
			lstObservers.get_Items().Clear();
			for (int i = 0; i < LunarObservations.OccMain.Observers.Count; i++)
			{
				lstObservers.get_Items().Add((object)LunarObservations.OccMain.Observers[i].ObserverName.ToString().Trim());
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
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_02e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02eb: Expected O, but got Unknown
			lstObservers = new ListBox();
			label1 = new Label();
			chkStarPath = new CheckBox();
			chkAll = new CheckBox();
			((Control)this).SuspendLayout();
			((ListControl)lstObservers).set_FormattingEnabled(true);
			((Control)lstObservers).set_Location(new Point(12, 43));
			((Control)lstObservers).set_Name("lstObservers");
			((Control)lstObservers).set_Size(new Size(195, 225));
			((Control)lstObservers).set_TabIndex(0);
			((Control)lstObservers).add_MouseDoubleClick(new MouseEventHandler(lstObservers_MouseDoubleClick));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(12, 28));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(192, 12));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("double-click to highlight Observer in the profile");
			((Control)chkStarPath).set_AutoSize(true);
			chkStarPath.set_Checked(Settings.Default.Graze_Observer_IncludeStarPath);
			((Control)chkStarPath).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Graze_Observer_IncludeStarPath", true, (DataSourceUpdateMode)1));
			((Control)chkStarPath).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkStarPath).set_Location(new Point(10, 11));
			((Control)chkStarPath).set_Name("chkStarPath");
			((Control)chkStarPath).set_Size(new Size(164, 17));
			((Control)chkStarPath).set_TabIndex(4);
			((Control)chkStarPath).set_Text("Include star path in graze plot");
			((ButtonBase)chkStarPath).set_UseVisualStyleBackColor(true);
			((Control)chkAll).set_AutoSize(true);
			((Control)chkAll).set_Location(new Point(179, 11));
			((Control)chkAll).set_Name("chkAll");
			((Control)chkAll).set_Size(new Size(37, 17));
			((Control)chkAll).set_TabIndex(5);
			((Control)chkAll).set_Text("All");
			((ButtonBase)chkAll).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(219, 276));
			((Control)this).get_Controls().Add((Control)(object)chkAll);
			((Control)this).get_Controls().Add((Control)(object)chkStarPath);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstObservers);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarListObservers", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarListObservers);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("ListCurrentObserversInProfile");
			((Control)this).set_Text("Highlight observers");
			((Form)this).add_Load((EventHandler)ListCurrentObserversInProfile_Load);
			((Form)this).add_Activated((EventHandler)ListCurrentObserversInProfile_Activated);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
