using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult;

namespace LightCurves
{
	public class PossibleDuplicateFiles : Form
	{
		private IContainer components;

		internal ListBox lstDups;

		public PossibleDuplicateFiles()
		{
			InitializeComponent();
		}

		private void PossibleDuplicateFiles_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
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
			lstDups = new ListBox();
			((Control)this).SuspendLayout();
			((Control)lstDups).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDups).set_FormattingEnabled(true);
			lstDups.set_ItemHeight(14);
			((Control)lstDups).set_Location(new Point(9, 9));
			((Control)lstDups).set_Name("lstDups");
			((Control)lstDups).set_Size(new Size(583, 60));
			((Control)lstDups).set_TabIndex(0);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((ScrollableControl)this).set_AutoScroll(true);
			((Form)this).set_ClientSize(new Size(602, 77));
			((Control)this).get_Controls().Add((Control)(object)lstDups);
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("PossibleDuplicateFiles");
			((Control)this).set_Text("Possible Duplicate Files");
			((Form)this).set_TopMost(true);
			((Form)this).add_Load((EventHandler)PossibleDuplicateFiles_Load);
			((Control)this).ResumeLayout(false);
		}
	}
}
