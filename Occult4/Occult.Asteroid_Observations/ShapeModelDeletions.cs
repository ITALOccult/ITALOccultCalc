using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class ShapeModelDeletions : Form
	{
		internal CheckBox[] DeleteSolution = (CheckBox[])(object)new CheckBox[14];

		private IContainer components;

		private Button cmdDelete;

		public ShapeModelDeletions()
		{
			InitializeComponent();
		}

		private void ShapeModelDeletions_Load(object sender, EventArgs e)
		{
			SetModelLines();
		}

		private void SetModelLines()
		{
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_0046: Expected O, but got Unknown
			for (int i = 0; i < EventDetails.ShapeData.Count; i++)
			{
				if (DeleteSolution[i] != null)
				{
					((Control)this).get_Controls().Remove((Control)(object)DeleteSolution[i]);
				}
			}
			for (int j = 0; j < EventDetails.ShapeData.Count; j++)
			{
				DeleteSolution[j] = new CheckBox();
				((Control)DeleteSolution[j]).set_Top(50 + 21 * j);
				((Control)DeleteSolution[j]).set_Left(10);
				((Control)DeleteSolution[j]).set_Width(700);
				((Control)DeleteSolution[j]).set_Text(EventDetails.ShapeData[j].ToString());
				((Control)DeleteSolution[j]).set_Font(new Font("Courier New", 8.25f));
				DeleteSolution[j].set_CheckAlign(ContentAlignment.MiddleLeft);
				((ButtonBase)DeleteSolution[j]).set_TextAlign(ContentAlignment.MiddleLeft);
				((Control)DeleteSolution[j]).set_Visible(true);
				((Control)this).get_Controls().Add((Control)(object)DeleteSolution[j]);
			}
			for (int k = EventDetails.ShapeData.Count; k < 14; k++)
			{
				if (DeleteSolution[k] != null)
				{
					((Control)DeleteSolution[k]).set_Visible(false);
				}
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			for (int num = EventDetails.ShapeData.Count - 1; num >= 0; num--)
			{
				if (DeleteSolution[num].get_Checked())
				{
					EventDetails.ShapeData.Remove(EventDetails.ShapeData[num]);
				}
			}
			EventDetails.ShapeModelFitted = EventDetails.ShapeData.Count > 0;
			SetModelLines();
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
			cmdDelete = new Button();
			((Control)this).SuspendLayout();
			((Control)cmdDelete).set_Location(new Point(39, 10));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(108, 27));
			((Control)cmdDelete).set_TabIndex(1);
			((Control)cmdDelete).set_Text("Delete selected");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(573, 250));
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("ShapeModelDeletions");
			((Control)this).set_Text("Shape Model Deletions");
			((Form)this).add_Load((EventHandler)ShapeModelDeletions_Load);
			((Control)this).ResumeLayout(false);
		}
	}
}
